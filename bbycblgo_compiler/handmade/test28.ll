; ModuleID = 'test28'
source_filename = "test28"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@COUNTER = global i32 0, align 4

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  store i32 1, ptr @COUNTER, align 4
  br label %loop.header

loop.header:                                      ; preds = %loop.body, %entry
  br label %loop.body

loop.body:                                        ; preds = %loop.header
  br label %loop.header

loop.exit:                                        ; No predecessors!
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
