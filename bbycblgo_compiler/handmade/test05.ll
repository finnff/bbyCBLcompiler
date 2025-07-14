; ModuleID = 'test05'
source_filename = "test05"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@END = global i32 0, align 4
@IF = global [2 x i8] zeroinitializer, align 1
@.str_lit0 = private unnamed_addr constant [5 x i8] c"TRUE\00", align 1
@.str_format_lit1 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str_lit2 = private unnamed_addr constant [5 x i8] c"TRUE\00", align 1
@.str_int = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str_newline3 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  store i32 10, ptr @END, align 4
  %0 = load i32, ptr @END, align 4
  %cmptmp = icmp eq i32 %0, 10
  br i1 %cmptmp, label %then, label %ifcont

then:                                             ; preds = %entry
  %1 = call i32 (ptr, ...) @printf(ptr @.str_format_lit1, i32 4, ptr @.str_lit2)
  %2 = load i32, ptr @END, align 4
  %3 = call i32 (ptr, ...) @printf(ptr @.str_int, i32 %2)
  %4 = call i32 (ptr, ...) @printf(ptr @.str_newline3)
  br label %ifcont

ifcont:                                           ; preds = %then, %entry
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
