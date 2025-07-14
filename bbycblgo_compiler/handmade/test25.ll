; ModuleID = 'test25'
source_filename = "test25"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@VAL = global i32 0, align 4
@.str_lit0 = private unnamed_addr constant [10 x i8] c"Odd prime\00", align 1
@.str_format_lit1 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str_lit2 = private unnamed_addr constant [10 x i8] c"Odd prime\00", align 1
@.str_newline3 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str_lit4 = private unnamed_addr constant [6 x i8] c"Other\00", align 1
@.str_format_lit5 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str_lit6 = private unnamed_addr constant [6 x i8] c"Other\00", align 1
@.str_newline7 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  store i32 5, ptr @VAL, align 4
  %0 = load i32, ptr @VAL, align 4
  %1 = load i32, ptr @VAL, align 4
  %cmptmp = icmp eq i32 %1, 3
  %2 = icmp eq i32 %0, 5
  %3 = or i1 %cmptmp, %2
  %4 = icmp eq i32 %0, 7
  %5 = or i1 %3, %4
  br i1 %5, label %then, label %ifcont

then:                                             ; preds = %entry
  %6 = call i32 (ptr, ...) @printf(ptr @.str_format_lit1, i32 9, ptr @.str_lit2)
  %7 = call i32 (ptr, ...) @printf(ptr @.str_newline3)
  br label %ifcont

ifcont:                                           ; preds = %then, %entry
  %8 = call i32 (ptr, ...) @printf(ptr @.str_format_lit5, i32 5, ptr @.str_lit6)
  %9 = call i32 (ptr, ...) @printf(ptr @.str_newline7)
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
