; ModuleID = 'test29'
source_filename = "test29"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
