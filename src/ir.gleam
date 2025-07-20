import op

pub type Ir {
  LoadInt(dest: Int, val: Int)
  LoadString(dest: Int, offset: Int)
  LoadName(dest: Int, name: String)

  BinaryOp(op: op.BinaryOp, dest: Int, src: Int)

  Not(dest: Int)
  Neg(dest: Int)

  Index(dest: Int, src: Int)
  RefIndex(dest: Int, src: Int)

  Load(ptr: Int)
  Store(ptr: Int, src: Int)

  GetArg(dest: Int, idx: Int)
  GetArgRef(dest: Int, idx: Int)

  GetAuto(dest: Int, idx: Int)
  SetAuto(src: Int, idx: Int)
  GetAutoRef(dest: Int, idx: Int)

  GetGlobal(dest: Int, idx: Int)
  SetGlobal(src: Int, idx: Int)
  GetGlobalRef(dest: Int, idx: Int)

  GetTempRef(dest: Int)

  PushArg(src: Int)
  Call(dest: Int)
  Return
}
