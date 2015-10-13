package com.proship.archsys

object Runner extends App {
  object Invocation extends Invocation(args)

  val volumes : List[Volume] = Invocation.volumes map (Volume(_, Invocation.mountAt))
  val reader = VolumeReader(Invocation.readerType, volumes)
  reader.read()
}