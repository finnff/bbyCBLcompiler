; ModuleID = '99bottles'
source_filename = "99bottles"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-redhat-linux-gnu"

@DATA = global i32 0, align 4
@goto_target_id_COUNT-BOTTLES = internal global i32 3
@.str_lit0 = private unnamed_addr constant [31 x i8] c"No bottles of beer on the wall\00", align 1
@.str_format1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str_lit2 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@.str_format3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str_lit4 = private unnamed_addr constant [34 x i8] c"Go to the store and buy some more\00", align 1
@.str_format5 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str_lit6 = private unnamed_addr constant [31 x i8] c"99 bottles of beer on the wall\00", align 1
@.str_format7 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str_lit8 = private unnamed_addr constant [27 x i8] c"bottle of beer on the wall\00", align 1
@.str_format9 = private unnamed_addr constant [6 x i8] c"%d%s\0A\00", align 1
@.str_lit10 = private unnamed_addr constant [28 x i8] c"bottles of beer on the wall\00", align 1
@.str_format11 = private unnamed_addr constant [6 x i8] c"%d%s\0A\00", align 1
@.str_lit12 = private unnamed_addr constant [16 x i8] c"bottles of beer\00", align 1
@.str_format13 = private unnamed_addr constant [6 x i8] c"%d%s\0A\00", align 1
@.str_lit14 = private unnamed_addr constant [30 x i8] c"Take one down, pass it around\00", align 1
@.str_format15 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str_lit16 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@.str_format17 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(ptr, ...)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i32 @main() {
entry:
  %DATA = alloca i32, align 4
  ret i32 0

NO-BOTTLES-LEFT:                                  ; preds = %SINGLE-BOTTLE
  %0 = call i32 (ptr, ...) @printf(ptr @.str_format1, ptr @.str_lit0)
  %1 = call i32 (ptr, ...) @printf(ptr @.str_format3, ptr @.str_lit2)
  %2 = call i32 (ptr, ...) @printf(ptr @.str_format5, ptr @.str_lit4)
  %3 = call i32 (ptr, ...) @printf(ptr @.str_format7, ptr @.str_lit6)
  ret i32 0

COUNT-BOTTLES:                                    ; preds = %ifcont, %loop.body
  br label %MANY-BOTTLES

SINGLE-BOTTLE:                                    ; No predecessors!
  %4 = load i32, ptr @DATA, align 4
  %5 = call i32 (ptr, ...) @printf(ptr @.str_format9, i32 %4, ptr @.str_lit8)
  br label %NO-BOTTLES-LEFT

MANY-BOTTLES:                                     ; preds = %COUNT-BOTTLES
  %6 = load i32, ptr @DATA, align 4
  %7 = call i32 (ptr, ...) @printf(ptr @.str_format11, i32 %6, ptr @.str_lit10)
  ret i32 0

END:                                              ; No predecessors!
  store i32 99, ptr %DATA, align 4
  br label %loop.header

loop.header:                                      ; preds = %after.perform1, %END
  %8 = load i32, ptr %DATA, align 4
  %loop.cond = icmp sle i32 %8, 0
  br i1 %loop.cond, label %loop.body, label %loop.exit

loop.body:                                        ; preds = %loop.header
  br label %COUNT-BOTTLES

loop.exit:                                        ; preds = %loop.header
  br label %loop.after

after.perform:                                    ; No predecessors!
  %9 = load i32, ptr %DATA, align 4
  %10 = call i32 (ptr, ...) @printf(ptr @.str_format13, i32 %9, ptr @.str_lit12)
  %11 = call i32 (ptr, ...) @printf(ptr @.str_format15, ptr @.str_lit14)
  %12 = load i32, ptr @DATA, align 4
  %subtmp = sub i32 %12, 1
  store i32 %subtmp, ptr @DATA, align 4
  %13 = load i32, ptr %DATA, align 4
  %14 = load i32, ptr %DATA, align 4
  %cmptmp = icmp eq i32 %14, 1
  br i1 %cmptmp, label %then, label %ifcont

then:                                             ; preds = %after.perform
  store i32 2, ptr @goto_target_id_COUNT-BOTTLES, align 4
  br label %ifcont

ifcont:                                           ; preds = %then, %after.perform
  br label %COUNT-BOTTLES

after.perform1:                                   ; No predecessors!
  %15 = call i32 (ptr, ...) @printf(ptr @.str_format17, ptr @.str_lit16)
  %16 = load i32, ptr %DATA, align 4
  %next.val = add i32 %16, 1
  store i32 %next.val, ptr %DATA, align 4
  br label %loop.header

loop.after:                                       ; preds = %loop.exit
  ret i32 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
