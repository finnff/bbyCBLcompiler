; ModuleID = 'fizzbuzz'
source_filename = "fizzbuzz"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@INT = global i32 0, align 4
@REM = global i32 0, align 4
@TMP = global i32 0, align 4
@.str_lit0 = private unnamed_addr constant [5 x i8] c"Fizz\00", align 1
@.str_format1 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str_lit2 = private unnamed_addr constant [5 x i8] c"Buzz\00", align 1
@.str_format3 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str_lit4 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@.str_format5 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str_format6 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  %INT = alloca i32, align 4
  store i32 100, ptr %INT, align 4
  br label %loop.header

loop.header:                                      ; preds = %ifcont, %entry
  %0 = load i32, ptr %INT, align 4
  %loop.cond = icmp sle i32 %0, 2147483647
  br i1 %loop.cond, label %loop.body, label %loop.exit

loop.body:                                        ; preds = %loop.header
  %1 = load i32, ptr %INT, align 4
  %quottmp = sdiv i32 %1, 3
  %remtmp = srem i32 %1, 3
  store i32 %quottmp, ptr @TMP, align 4
  store i32 %remtmp, ptr @REM, align 4
  %2 = load i32, ptr @REM, align 4
  %3 = load i32, ptr @REM, align 4
  %cmptmp = icmp eq i32 %3, 0
  br i1 %cmptmp, label %then, label %ifcont

loop.exit:                                        ; preds = %loop.header
  br label %loop.after

then:                                             ; preds = %loop.body
  %4 = call i32 (ptr, ...) @printf(ptr @.str_format1, ptr @.str_lit0)
  %5 = load i32, ptr %INT, align 4
  %quottmp1 = sdiv i32 %5, 5
  %remtmp2 = srem i32 %5, 5
  store i32 %quottmp1, ptr @TMP, align 4
  store i32 %remtmp2, ptr @REM, align 4
  %6 = load i32, ptr @REM, align 4
  %7 = load i32, ptr @REM, align 4
  %cmptmp3 = icmp eq i32 %7, 0
  br i1 %cmptmp3, label %then4, label %ifcont5

ifcont:                                           ; preds = %ifcont5, %loop.body
  %8 = load i32, ptr %INT, align 4
  %next.val = add i32 %8, 1
  store i32 %next.val, ptr %INT, align 4
  br label %loop.header

then4:                                            ; preds = %then
  %9 = call i32 (ptr, ...) @printf(ptr @.str_format3, ptr @.str_lit2)
  %10 = load i32, ptr %INT, align 4
  %quottmp6 = sdiv i32 %10, 15
  %remtmp7 = srem i32 %10, 15
  store i32 %quottmp6, ptr @TMP, align 4
  store i32 %remtmp7, ptr @REM, align 4
  %11 = load i32, ptr @REM, align 4
  %12 = load i32, ptr @REM, align 4
  %cmptmp8 = icmp eq i32 %12, 0
  br i1 %cmptmp8, label %then9, label %else

ifcont5:                                          ; preds = %ifcont10, %then
  br label %ifcont

then9:                                            ; preds = %then4
  %13 = call i32 (ptr, ...) @printf(ptr @.str_format5, ptr @.str_lit4)
  br label %ifcont10

ifcont10:                                         ; preds = %else, %then9
  br label %ifcont5

else:                                             ; preds = %then4
  %14 = load i32, ptr %INT, align 4
  %15 = call i32 (ptr, ...) @printf(ptr @.str_format6, i32 %14)
  br label %ifcont10

loop.after:                                       ; preds = %loop.exit
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
