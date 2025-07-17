; ModuleID = 'test27'
source_filename = "test27"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@NUM = global i32 0, align 4
@QUOT = global i32 0, align 4
@REM = global i32 0, align 4
@.str_lit0 = private unnamed_addr constant [10 x i8] c"Remainder\00", align 1
@.str_format1 = private unnamed_addr constant [6 x i8] c"%s%d\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  store i32 10, ptr @NUM, align 4
  %0 = load i32, ptr @NUM, align 4
  %quottmp = sdiv i32 %0, 3
  %remtmp = srem i32 %0, 3
  store i32 %quottmp, ptr @QUOT, align 4
  store i32 %remtmp, ptr @REM, align 4
  %1 = load i32, ptr @REM, align 4
  %2 = call i32 (ptr, ...) @printf(ptr @.str_format1, ptr @.str_lit0, i32 %1)
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
