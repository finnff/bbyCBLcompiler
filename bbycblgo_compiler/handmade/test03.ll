; ModuleID = 'test03'
source_filename = "test03"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@.str_format_lit0 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str_lit1 = private unnamed_addr constant [20 x i8] c"This is a very long\00", align 1
@.str_format_lit2 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str_lit3 = private unnamed_addr constant [26 x i8] c" string concatenated with\00", align 1
@.str_format_lit4 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str_lit5 = private unnamed_addr constant [25 x i8] c" continuation characters\00", align 1
@.str_newline6 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  %0 = call i32 (ptr, ...) @printf(ptr @.str_format_lit0, i32 19, ptr @.str_lit1)
  %1 = call i32 (ptr, ...) @printf(ptr @.str_format_lit2, i32 25, ptr @.str_lit3)
  %2 = call i32 (ptr, ...) @printf(ptr @.str_format_lit4, i32 24, ptr @.str_lit5)
  %3 = call i32 (ptr, ...) @printf(ptr @.str_newline6)
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
