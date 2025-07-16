; ModuleID = 'test16'
source_filename = "test16"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@NAME = global [5 x i8] zeroinitializer, align 1
@.str_lit0 = private unnamed_addr constant [6 x i8] c"Hello\00", align 1
@.str1 = private unnamed_addr constant [6 x i8] c"Hello\00", align 1
@.str_format_var2 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str_newline3 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  call void @llvm.memcpy.p0.p0.i64(ptr @NAME, ptr @.str1, i64 5, i1 false)
  %0 = call i32 (ptr, ...) @printf(ptr @.str_format_var2, i32 5, ptr @NAME)
  %1 = call i32 (ptr, ...) @printf(ptr @.str_newline3)
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
