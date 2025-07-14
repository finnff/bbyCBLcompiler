; ModuleID = 'test12'
source_filename = "test12"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@STUDENTNAME = global [2 x i8] zeroinitializer, align 1
@STUDENT_AGE = global i32 0, align 4
@.str_lit0 = private unnamed_addr constant [3 x i8] c"AB\00", align 1
@.str1 = private unnamed_addr constant [3 x i8] c"AB\00", align 1
@.str_int = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str_newline2 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr @STUDENTNAME, ptr @.str1, i64 2, i1 false)
  %0 = load i32, ptr @STUDENT_AGE, align 4
  %1 = call i32 (ptr, ...) @printf(ptr @.str_int, i32 %0)
  %2 = call i32 (ptr, ...) @printf(ptr @.str_newline2)
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
