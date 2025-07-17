; ModuleID = 'test41'
source_filename = "test41"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@ARR = global [3 x i32] zeroinitializer, align 4
@.str_format0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  store i32 10, ptr @ARR, align 4
  store i32 10, ptr getelementptr inbounds ([3 x i32], ptr @ARR, i32 0, i32 1), align 4
  %I = alloca i32, align 4
  store i32 10, ptr getelementptr inbounds ([3 x i32], ptr @ARR, i32 0, i32 2), align 4
  store i32 1, ptr %I, align 4
  br label %loop.header

loop.header:                                      ; preds = %loop.body, %entry
  %0 = load i32, ptr %I, align 4
  %loop.cond = icmp sle i32 %0, 3
  br i1 %loop.cond, label %loop.body, label %loop.exit

loop.body:                                        ; preds = %loop.header
  %1 = load i32, ptr %I, align 4
  %idx.adj = sub i32 %1, 1
  %2 = getelementptr inbounds [3 x i32], ptr @ARR, i32 0, i32 %idx.adj
  %3 = load i32, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr @.str_format0, i32 %3)
  %5 = load i32, ptr %I, align 4
  %next.val = add i32 %5, 1
  store i32 %next.val, ptr %I, align 4
  br label %loop.header

loop.exit:                                        ; preds = %loop.header
  br label %loop.after

loop.after:                                       ; preds = %loop.exit
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
