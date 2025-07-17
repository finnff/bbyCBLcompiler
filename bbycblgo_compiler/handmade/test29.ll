; ModuleID = 'test29'
source_filename = "test29"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@.str_perform0 = private unnamed_addr constant [5 x i8] c"Loop\00", align 1
@.str_format1 = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@.str_lit2 = private unnamed_addr constant [5 x i8] c"Loop\00", align 1
@.str_format3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  ret i32 0

START:                                            ; preds = %LOOPPARA
  %perform.counter = alloca i32, align 4
  store i32 0, ptr %perform.counter, align 4
  br label %perform.header

LOOPPARA:                                         ; No predecessors!
  %0 = call i32 (ptr, ...) @printf(ptr @.str_format3, ptr @.str_lit2)
  br label %START

perform.header:                                   ; preds = %perform.body, %START
  %1 = load i32, ptr %perform.counter, align 4
  %perform.cond = icmp slt i32 %1, 3
  br i1 %perform.cond, label %perform.body, label %perform.exit

perform.body:                                     ; preds = %perform.header
  %2 = call i32 (ptr, ...) @printf(ptr @.str_format1, i32 4, ptr @.str_perform0)
  %3 = load i32, ptr %perform.counter, align 4
  %4 = add i32 %3, 1
  store i32 %4, ptr %perform.counter, align 4
  br label %perform.header

perform.exit:                                     ; preds = %perform.header
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
