package com.proship.archsys

object Volume {
  def apply(mountPath: String, mountAt: String) : Volume = {
    val mounts : Array[String] = Sys.mounts split "\n"

    val filtered = mounts filter (
      line => {
        val fields = line split " +" map (_.trim)
        fields(2) == mountPath
      })

    val mountInfo: Array[(String,String,String)] = filtered map (
      line => {
        val fields = line split " +" map (_.trim)
        (fields(0), fields(2), fields(4))
      })

    if (mountInfo.isEmpty)
      throw new IllegalArgumentException("nothing mounted at "+mountPath)

    def stringify(mounts: Array[(String,String,String)]) =
      mounts map (m => m._1+"@"+m._2+" ("+m._3+")") reduce (_+"\n"+_)

    if (mountInfo.length > 1)
      throw new IllegalStateException(
        "more than one thing mounted at "+mountPath+".\n"+
          "ambiguous host configuration.\n"+
          stringify(mountInfo))

    val (dev, path, mountType) = mountInfo(0)

    if (Sys.isLvmManaged(dev))
      new LvmVolume(dev, path, mountAt)
    else if (Sys.isBtrfsVolume(dev))
      new BtrfsVolume(dev, path, mountAt)
    else if (Sys.isBlockDevice(dev))
      new RawVolume(dev, path, mountAt)
    else
      throw new IllegalArgumentException("not a real block device: "+dev)
  }

  private class LvmVolume(dev: String, path: String, mountAt: String) extends Volume(dev, path, mountAt) {
    val (vg, liveLv, liveLvSize) = Sys.getLvInfo(dev)
    val snapLv = liveLv+"-snap"
    val snapDev = "/dev/"+vg+"/"+snapLv
    val snapLvSize = calculateLvSnapSize(liveLvSize)

    private def calculateLvSnapSize(originalLvSize: String) = {
      val portion = 0.25
      val sizeAndUnit = originalLvSize.split("\\.\\d+")
      ((sizeAndUnit(0).toFloat * portion) toString()) + sizeAndUnit(1)
    }
    protected def doAcquire() = Sys.createLvSnap(vg, liveLv, snapLv, snapLvSize)
    protected def doRelease() = Sys.destroyLv(vg, snapLv)
    protected def doMount() = Sys.mount(snapDev, mountAt+path)
    protected def doUnmount() = Sys.unmount(mountAt+path)

    override def getDevForReading = if (Sys.isLvmManaged(snapDev)) snapDev else super.getDevForReading

    override def toString = super.toString+":vg="+vg+":lv="+liveLv
  }

  private class BtrfsVolume(dev: String, path: String, mountAt: String) extends Volume(dev, path, mountAt) {
    protected def doAcquire() = Sys.createBtrfsSnap(path, mountAt)
    protected def doRelease() = Sys.destroyBtrfsSnap(mountAt)
    protected def doMount() = {}
    protected def doUnmount() = {}
  }

  private class RawVolume(dev: String, path: String, mountAt: String) extends Volume(dev, path, mountAt) {
    protected def doAcquire() = ()
    protected def doRelease() = ()
    protected def doMount() = Sys.mount(path, mountAt+path)
    protected def doUnmount() = Sys.unmount(mountAt+path)
  }
}

/**
 * Basic archival volume abstraction.
 * Can be acquired and released.
 */
abstract class Volume(dev: String, path: String, mountAt: String) {
  private var acquired = false
  private var mounted = false

  protected def doAcquire() : Unit
  protected def doRelease() : Unit
  protected def doMount() : Unit
  protected def doUnmount() : Unit

  def getDevForReading = dev

  def getMountLocation = mountAt

  def acquire() {
    acquired = { doAcquire(); true }
    if (mountAt != null) {
      mounted = { doMount(); true }
    }
  }
  def close () {
    if (mounted) {
      mounted = { doUnmount(); false }
    }
    if (acquired) {
      acquired = { doRelease(); false }
    }
  }

  override def toString = getClass+":"+dev+"@"+path
}