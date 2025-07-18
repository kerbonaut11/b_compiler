pub type Ir {
  LoadInt(dest: Int, val: Int)
  LoadString(dest: Int, offset: Int)

  Add(dest: Int, src: Int)
  Sub(dest: Int, src: Int)
  Mul(dest: Int, src: Int)
  Div(dest: Int, src: Int)
  Mod(dest: Int, src: Int)

  And(dest: Int, src: Int)
  Or(dest: Int, src: Int)
  Xor(dest: Int, src: Int)
  Shl(dest: Int, src: Int)
  Shr(dest: Int, src: Int)

  Not(dest: Int)
  Neg(dest: Int)

  Index(dest: Int, src: Int)
  RefIndex(dest: Int, src: Int)

  Load(ptr: Int)
  Store(ptr: Int, src: Int)

  GetArg(dest: Int, idx: Int)
  GetArgRef(dest: Int, idx: Int)

  GetAuto(dest: Int, idx: Int)
  SetAuto(dest: Int, idx: Int)
  GetAutoRef(dest: Int, idx: Int)

  GetGlobal(dest: Int, idx: Int)
  SetGlobal(dest: Int, idx: Int)
  GetGlobalRef(dest: Int, idx: Int)

  GetName(dest: Int, name: String)

  SetArg(src: Int)
  Call(dest: Int)
  Return
}
