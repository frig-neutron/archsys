#!/bin/bash
PATH=$PATH:/sbin:/usr/sbin
#  pgrep -f $0\ $1 | grep -v grep | grep -v pgrep | grep -v $$ > /dev/null
#  if [[ $? -eq 0 ]]; then
#    echo process running
#    exit 3
#  fi

  exec scala "$0" "$@"
  wait
!#

import scala.sys.process._

var lvmUtilsPrefix = ""
var volumeGroupName = ""
var machineName = ""

object ArchSys
{
	val logStdStuff = ProcessLogger(
		(s: String) => info(s),
		(s: String) => error(s)
	)

	class Log(level: String) 
	{
		val tag = "update-backup-all"
		def loggerCmd(msg: String) = 
		{
			val logCmd = ("logger -t " + tag + " -- " + level + " - " + msg)
			logCmd ! 
		}

		def apply(msg: String) = loggerCmd (msg)
	}

	val info = new Log("INFO")
	val debug = new Log("DEBUG")
	val error = new Log("ERROR")
	val fatal = new Log("FATAL")

	val (lvName, mountPointSuffix) = ("lvName", "mountPointSuffix")

	val filesystemsMap = Map (
		"iron-system-image" -> List(
			Map(lvName -> "root", mountPointSuffix -> "/"),
			Map(lvName -> "usr", mountPointSuffix -> "/usr"),
			Map(lvName -> "var", mountPointSuffix -> "/var" ),
			Map(lvName -> "artifactory", mountPointSuffix -> "/usr/local/artifactory" ),
			Map(lvName -> "home", mountPointSuffix -> "/home" )
		),

		"iron-act-image" -> List(
			Map(lvName -> "act-partition", mountPointSuffix -> "")
		),

		"warden-system-image" -> List(
			Map(lvName -> "root", mountPointSuffix -> "/"),
			Map(lvName -> "usr", mountPointSuffix -> "/usr"),
			Map(lvName -> "var", mountPointSuffix -> "/var" ),
			Map(lvName -> "home", mountPointSuffix -> "/home" )
		),

		"domU-centos55-mailtest" -> List(
			Map(lvName -> "domU-centos55-mailtest", mountPointSuffix -> "/")
		),

		"domU-omrs-int" -> List(
			Map(lvName -> "domU-omrs-int", mountPointSuffix -> "/")
		),

		"domU-omrs-int-omrs" -> List(
			Map(lvName -> "domU-omrs-int-omrs", mountPointSuffix -> "/")
		),

		"net-services-system-image" -> List(
			Map(lvName -> "net-services-disk", mountPointSuffix -> "/")
		)
 	)
  
	def prepareVars = 
	{
		lvmUtilsPrefix = (("which lvremove") !!).toString.trim.replace("sbin/lvremove", "")
//		val hostName = (("hostname") !!).toString.trim
//		volumeGroupName = getVGName(hostName)
	}

	def getVGName =
	{
		machineName match
		{
			case "warden-system-image"		=>	"vg0"
			case "iron-act-image"			=>	"WORK"
			case "iron-main-image"			=>	"WORK"
			case "iron-system-image"		=>	"mother"
			case "domU-centos55-mailtest"		=>	"mother"
			case "domU-omrs-int"			=>	"mother"
			case "domU-omrs-int-omrs"		=>	"mother"
			case "net-services-system-image"	=>	"mother"
			case "test"				=>	"system"
		}
	}
  
	def main(args: Array[String]) : Unit = 
	{
		prepareVars
		sanityCheck(args)
		machineName = (args(0)).trim
		backup(filesystemsMap(machineName))
	}

	val excludes = List("/dev", "/var/spool", "/lost+found", "/mnt", "/proc", "/sys", "/var/spool/squid")
	val snapshotRootMountPoint = "/mnt/snapshot"
	
	val snapshotNameSuffix = "-snap"
	val snapSize = "256M"
	
	val FATAL_LVM_CREATE = 4
	val FATAL_MOUNT = 5
	val ERROR_UNMOUNT = 6
	val ERROR_LVM_REMOVE = 7
	val FATAL_SCRIPT_PARAMETERS = 8
	val FATAL_NO_MACHINE_TO_BACKUP = 9
	val FATAL_SYSTEM_SHUTDOWN = 10
	val FATAL_SYSTEM_REBOOT = 11
	val FATAL_NO_SPACE = 12

	def backup(filesystems: List[Map[String, String]])
	{
		snapshotTargetVolumes(filesystems)
		mountTargetVolumes(filesystems)
		pipeOutput(filesystems, new java.io.BufferedOutputStream(System.out, 65536))
		unmountTargetVolumes(filesystems)
		removeTargetVolumeSnapshots(filesystems)
		System.exit(0) 
	}

	private def sanityCheck(args: Array[String]) =
	{ 
		//Check that the minium number of parameters passed to the script are correct
		if ( args.size < 1 )
		{
			fatal ("script usage: archsys.scala name-of-system-to-backup")
			System.exit(FATAL_SCRIPT_PARAMETERS)
		}
  
		//System should not be shutting down
		val runLevel = ("runlevel"!!).split("\\s+")(1)
		if (runLevel == 0)
		{
			fatal ("shutdown of system in progress. Aborting backup.")
			System.exit(FATAL_SYSTEM_SHUTDOWN)
		}
		else if ( runLevel == 6 )
		{
			fatal ("reboot of system in progress. Aborting backup.")
			System.exit(FATAL_SYSTEM_REBOOT)
		}      

		//Volumemap should contain key for machine to backup
		val machineName = (args(0)).trim
		if(!filesystemsMap.contains(machineName))
		{
			fatal("System " + machineName + " could not be found to back up.  Aborting backup.");
			System.exit(FATAL_NO_MACHINE_TO_BACKUP);
		}
  
/*		//There should be enough free space for snapshots
		val freeSpace = ("df /dev"!!).split("\\s+")(10)
		val needSpace = (256000 * filesystemsMap(machineName).size).toString
		if (freeSpace < needSpace)
		{
			fatal("Not enough space to make snapshots for: " + machineName + ". Needed: " + needSpace + " bytes, Found: " + freeSpace + " bytes free.")
			System.exit(FATAL_NO_SPACE)
		}*/
	}

	private def snapshotTargetVolumes(filesystems: List[Map[String, String]]) = 
	{
		info ("> create volume snapshots")
		def snapshotVol(volDef : Map[String, String]) = 
		{
			val volDevice = volumeDevicePath(volDef)
			val snapLv = volDef(lvName)+snapshotNameSuffix
			val cmd = lvmUtilsPrefix+"sbin/lvcreate -L " + snapSize + " -s -n " + snapLv + " " + volDevice
			val errCode = cmd ! logStdStuff
			if (errCode != 0 )
			{
				fatal ("create snapshot of volume: " + volDevice)
				unmountTargetVolumes(filesystems)
				removeTargetVolumeSnapshots(filesystems)
				System.exit(FATAL_LVM_CREATE)
			}
		}
		filesystems map snapshotVol
		info ("< create volume snapshots")
	}

	private def volumeDevicePath(volDef: Map[String,String]) = "/dev/" + getVGName + "/" + volDef(lvName)
	private def mountDevicePath(volDef: Map[String, String]) = volumeDevicePath(volDef) + snapshotNameSuffix

	private def removeTargetVolumeSnapshots(filesystems: List[Map[String, String]]) =
	{
		info ("> remove volume snapshots");
		def snapshotVol(volDef : Map[String, String]) = 
		{
			val volDevice = getVGName + "/" + volDef(lvName) + snapshotNameSuffix
			val cmd = lvmUtilsPrefix + "sbin/lvremove -f " + volDevice
			val errCode = cmd ! logStdStuff  
			if (errCode != 0 )
			{
				fatal ("removing snapshot: " + volDevice)
				System.exit(ERROR_LVM_REMOVE)
			}
		}
		filesystems map snapshotVol
		info ("< remove volume snapshots")
	}

	private def isMounted(loc: String) = 
	{
		Process("mount").lines.exists(_.split("\\s+")(2) == loc)
	}
  
	private def mountTargetVolumes(filesystems: List[Map[String, String]]) =
	{
		info ("> mount volume snapshots")
		def mountLVM( volDef:Map[String, String] ) =
		{			
			val volume = mountDevicePath(volDef)
			if (!volDef(mountPointSuffix).equals(""))
			{
				val mountPoint = snapshotRootMountPoint + volDef(mountPointSuffix).replaceAll("/$", "")
				info ("mount attempt of volume: " + volume + " on mount point: " + mountPoint)
				val mountCmd = "/bin/mount -oro " + volume + " " + mountPoint 
				val errCode = mountCmd ! logStdStuff
				if (errCode != 0 )
				{
					fatal ("mounting volume: " + volume + " at mountpoint: " + mountPoint + "  with command: " + mountCmd)
					unmountTargetVolumes(filesystems)
					removeTargetVolumeSnapshots(filesystems)
					System.exit(FATAL_MOUNT)
				}
			}
			else
			{
				info("volume: " + volume + "has no defined mount point, skipping mounting of volume");
			}
		}
		filesystems map mountLVM
		info ("< mount volume snapshots")
	}
  
	private def unmountTargetVolumes(filesystems: List[Map[String, String]]) =
	{
		info ("> unmount volume snapshots")
		def unmountLVM(volDef: Map[String, String] ) =
		{
			val volume = mountDevicePath(volDef)
			if (!volDef(mountPointSuffix).equals(""))
			{
				val snapshotMountLoc = snapshotRootMountPoint + volDef(mountPointSuffix).replaceAll("/$", "")
				info ("unmount attempt of volume: " + volume + " mounted at: " + snapshotMountLoc)
				if (isMounted(snapshotMountLoc))
				{
					unmount(snapshotMountLoc)
				}
				else
				{
					info("no volume: " + volume + " mounted at: " + snapshotMountLoc + ": skipping")
				}
			}
			else
			{
				info("volume: " + volume + "has no defined mount point, skipping unmount of volume");
			}			
		}
		filesystems.reverse map unmountLVM
		info ("< unmount volume snapshots")
	}

	private def unmount(loc: String) = 
	{
		val unmountCmd = "umount " + loc 
		val errCode = unmountCmd ! logStdStuff
		if (errCode != 0 )
		{
			error("unmounting at:  " + loc + " failed")
		}
	}

	private def tar(out: java.io.OutputStream) = 
	{
		info ("> create tar of volume snapshots")
		val mkExcludes = excludes map ( "--exclude=" + snapshotRootMountPoint + _ + "/* " ) mkString
		val tarCmd = "tar -cz " + mkExcludes + " -C " + snapshotRootMountPoint + "/.. " + snapshotRootMountPoint 
		info ("attemting tar of snapshot filesystem with command: " + tarCmd)
		(Process(tarCmd) #> out).run.exitValue
		info ("< create tar of volume snapshots")
	}

	private def lzma(volDef: Map[String, String], out: java.io.OutputStream) = 
	{
		val sourceSnapShotPath = mountDevicePath(volDef)
		info ("> create lzma of volume snapshots")
		val lzmaCmd = "/bin/cat " + sourceSnapShotPath + " |xz --format=lzma -c --lzma1=dict=128M -5 " 
		info ("attemting tar of snapshot filesystem with command: " + lzmaCmd)
		(Process(lzmaCmd) #> out).run.exitValue
		info ("< create tar of volume snapshots")
	}

	private def pipeOutput(filesystems: List[Map[String, String]], out: java.io.OutputStream)
	{
		info ("> pipe of output")
		def chooseOutputPipe( volDef:Map[String, String] ) =
                {
			if(volDef(lvName).contains("act"))
			{	
				lzma(volDef, out)
			}
			else
			{
				tar(out)
			}
		}
		filesystems map chooseOutputPipe
		info ("< pipe of output")
	}
}

ArchSys.main(args)
