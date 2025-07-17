; ModuleID = 'test30'
source_filename = "test30"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@goto_target_id_PATH = internal global i32 2
@.str_lit0 = private unnamed_addr constant [8 x i8] c"Skipped\00", align 1
@.str_format1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str_lit2 = private unnamed_addr constant [8 x i8] c"Altered\00", align 1
@.str_format3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  ret i32 0

PATH:                                             ; preds = %MAIN
  br label %SKIP

MAIN:                                             ; preds = %MAIN
  store i32 3, ptr @goto_target_id_PATH, align 4
  %current_goto_target_id = load i32, ptr @goto_target_id_PATH, align 4
  switch i32 %current_goto_target_id, label %goto.default [
    i32 0, label %PATH
    i32 1, label %MAIN
    i32 2, label %SKIP
    i32 3, label %DONE
  ]

SKIP:                                             ; preds = %MAIN, %PATH
  %0 = call i32 (ptr, ...) @printf(ptr @.str_format1, ptr @.str_lit0)
  ret i32 0

DONE:                                             ; preds = %MAIN
  %1 = call i32 (ptr, ...) @printf(ptr @.str_format3, ptr @.str_lit2)
  ret i32 0

goto.default:                                     ; preds = %MAIN
  ret i32 1
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
