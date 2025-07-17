; ModuleID = 'test36'
source_filename = "test36"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@A = global i32 0, align 4
@B = global i32 0, align 4
@RES = global i32 0, align 4
@.str_format0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  store i32 10, ptr @A, align 4
  store i32 10, ptr @B, align 4
  %0 = load i32, ptr @A, align 4
  %subtmp = sub i32 %0, 2
  store i32 %subtmp, ptr @A, align 4
  %1 = load i32, ptr @RES, align 4
  %2 = call i32 (ptr, ...) @printf(ptr @.str_format0, i32 %1)
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
