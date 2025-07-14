; ModuleID = 'test31'
source_filename = "test31"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@INPUTVAL = global i32 0, align 4
@.str_int = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str_newline0 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  %0 = load i32, ptr @INPUTVAL, align 4
  %1 = load i32, ptr @INPUTVAL, align 4
  %2 = call i32 (ptr, ...) @printf(ptr @.str_int, i32 %1)
  %3 = call i32 (ptr, ...) @printf(ptr @.str_newline0)
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
