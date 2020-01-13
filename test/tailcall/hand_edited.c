#include "kernel.h"
enum {
  JS_Browser_element,
  JS_Json_succeed,
  JS_Platform_batch,
  JS_VirtualDom_node,
  JS_VirtualDom_text,
};
enum {
  FIELD_init,
  FIELD_subscriptions,
  FIELD_update,
  FIELD_view,
};
Closure VirtualDom_text = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_VirtualDom_text,
};
Closure VirtualDom_node = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_VirtualDom_node,
};
Closure Platform_batch = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Platform_batch,
};
Closure Json_succeed = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Json_succeed,
};
Closure Browser_element = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Browser_element,
};
FieldGroup fg_init_update_view = {
    .size = 3,
    .fields =
        {
            FIELD_init,
            FIELD_update,
            FIELD_view,
        },
};
FieldGroup fg_init_subscriptions_update_view = {
    .size = 4,
    .fields =
        {
            FIELD_init,
            FIELD_subscriptions,
            FIELD_update,
            FIELD_view,
        },
};
ElmString16 literal_string_div = {
    .header = HEADER_STRING(3),
    .words16 =
        {
            0x64,
            0x69,
            0x76,
        },
};
ElmInt literal_int_0 = {
    .header = HEADER_INT,
    .value = 0,
};
ElmInt literal_int_1 = {
    .header = HEADER_INT,
    .value = 1,
};
ElmInt literal_int_10 = {
    .header = HEADER_INT,
    .value = 10,
};

FieldGroup* app_field_groups[] = {
    &fg_init_subscriptions_update_view,
    &fg_init_update_view,
    NULL,
};

#define author_project_Main_initialModel Unit

#define elm_core_Platform_Cmd_batch Platform_batch
#define elm_core_Platform_Cmd_none (*ptr_elm_core_Platform_Cmd_none)
ElmValue* ptr_elm_core_Platform_Cmd_none;
void* init_elm_core_Platform_Cmd_none() {
  return A1(&elm_core_Platform_Cmd_batch, &Nil);
}

#define elm_core_Platform_Sub_batch Platform_batch
#define elm_core_Platform_Sub_none (*ptr_elm_core_Platform_Sub_none)
ElmValue* ptr_elm_core_Platform_Sub_none;
void* init_elm_core_Platform_Sub_none() {
  return A1(&elm_core_Platform_Sub_batch, &Nil);
}
void* eval_elm_browser_Browser_sandbox_lambda0(void* args[]) {
  void* x_impl = args[0];
  void* x_msg = args[1];
  void* x_model = args[2];
  return NEW_TUPLE2(A2(Utils_access_eval(((void* []){
                           (void*)FIELD_update,
                           x_impl,
                       })),
                        x_msg,
                        x_model),
      &elm_core_Platform_Cmd_none);
}
void* eval_elm_browser_Browser_sandbox_lambda1(void* args[]) {
  void* x__v1 = args[0];
  return &elm_core_Platform_Sub_none;
}
void* eval_elm_browser_Browser_sandbox_lambda2(void* args[]) {
  void* x_impl = args[0];
  void* x__v0 = args[1];
  return NEW_TUPLE2(Utils_access_eval(((void* []){
                        (void*)FIELD_init,
                        x_impl,
                    })),
      &elm_core_Platform_Cmd_none);
}
void* eval_elm_browser_Browser_sandbox(void* args[]) {
  void* x_impl = args[0];
  return A1(&Browser_element,
      NEW_RECORD(&fg_init_subscriptions_update_view,
          4,
          ((void* []){
              NEW_CLOSURE(1,
                  2,
                  &eval_elm_browser_Browser_sandbox_lambda2,
                  ((void* []){
                      x_impl,
                  })),
              NEW_CLOSURE(
                  0, 1, &eval_elm_browser_Browser_sandbox_lambda1, ((void* []){})),
              NEW_CLOSURE(1,
                  3,
                  &eval_elm_browser_Browser_sandbox_lambda0,
                  ((void* []){
                      x_impl,
                  })),
              Utils_access_eval(((void* []){
                  (void*)FIELD_view,
                  x_impl,
              })),
          })));
}
Closure elm_browser_Browser_sandbox = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_browser_Browser_sandbox,
};

#define elm_json_Json_Decode_succeed Json_succeed

void* eval_author_project_Main_update(void* args[]) {
  void* x__v0 = args[0];
  void* x__v1 = args[1];
  return &Unit;
}
Closure author_project_Main_update = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_author_project_Main_update,
};

void* eval_elm_core_Basics_apL(void* args[]) {
  void* x_f = args[0];
  void* x_x = args[1];
  return A1(x_f, x_x);
}
Closure elm_core_Basics_apL = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_core_Basics_apL,
};

#define elm_html_Html_div (*ptr_elm_html_Html_div)
ElmValue* ptr_elm_html_Html_div;
void* init_elm_html_Html_div() {
  return A1(&VirtualDom_node, &literal_string_div);
}

#define elm_core_String_fromInt String_fromNumber

#define elm_core_Basics_add Basics_add

#define elm_core_Basics_gt Utils_gt

void* tce_author_project_Main_tailCallWrapperFunc_lambda0(
    void* args[], void** gc_tce_data) {
tce_loop:
  void* x_xx = args[0];
  void* tmp1;
  if (A2(&Utils_gt, x_xx, &literal_int_10) == &True) {
    tmp1 = x_xx;
  } else {
    void* tmp2 = A2(&Basics_add, x_xx, literal_int_1);
    *gc_tce_data = CAN_THROW(GC_tce_iteration(1));
    args[0] = tmp2;
    goto tce_loop;
    tmp1 = NULL;  // 'if' code gen wants to assign tmp1 here but it's unreachable
  }
  return tmp1;
}
void* eval_author_project_Main_tailCallWrapperFunc_lambda0(void* args[]) {
  return GC_tce_eval(&tce_author_project_Main_tailCallWrapperFunc_lambda0,
      &eval_author_project_Main_tailCallWrapperFunc_lambda0,
      1,
      args);
}

void* eval_author_project_Main_tailCallWrapperFunc(void* args[]) {
  void* x_x = args[0];
  void* x_tailFunc = NEW_CLOSURE(
      0, 1, &eval_author_project_Main_tailCallWrapperFunc_lambda0, ((void* []){}));
  return A1(x_tailFunc, x_x);
}
Closure author_project_Main_tailCallWrapperFunc = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_author_project_Main_tailCallWrapperFunc,
};

#define elm_virtual_dom_VirtualDom_text VirtualDom_text
#define elm_html_Html_text elm_virtual_dom_VirtualDom_text

void* tce_author_project_Main_topLevelTailFunc(void* args[], void** gc_tce_data) {
tce_loop:
  void* x_xx = args[0];
  void* tmp1;
  if (A2(&Utils_gt, x_xx, &literal_int_10) == &True) {
    tmp1 = x_xx;
  } else {
    void* tmp2 = A2(&Basics_add, x_xx, literal_int_1);
    *gc_tce_data = CAN_THROW(GC_tce_iteration(1));
    args[0] = tmp2;
    goto tce_loop;
    tmp1 = NULL;  // 'if' code gen wants to assign tmp1 here but it's unreachable
  }
  return tmp1;
}
void* eval_author_project_Main_topLevelTailFunc(void* args[]) {
  return GC_tce_eval(&tce_author_project_Main_topLevelTailFunc,
      &eval_author_project_Main_topLevelTailFunc,
      1,
      args);
}
Closure author_project_Main_topLevelTailFunc = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_author_project_Main_topLevelTailFunc,
};

void* eval_author_project_Main_view(void* args[]) {
  void* x__v0 = args[0];
  return A2(&elm_html_Html_div,
      &Nil,
      List_fromArray(2,
          ((void* []){
              A2(&elm_html_Html_div,
                  &Nil,
                  List_fromArray(1,
                      ((void* []){
                          A2(&elm_core_Basics_apL,
                              &elm_html_Html_text,
                              A1(&elm_core_String_fromInt,
                                  A1(&author_project_Main_tailCallWrapperFunc,
                                      &literal_int_0))),
                      }))),
              A2(&elm_html_Html_div,
                  &Nil,
                  List_fromArray(1,
                      ((void* []){
                          A2(&elm_core_Basics_apL,
                              &elm_html_Html_text,
                              A1(&elm_core_String_fromInt,
                                  A1(&author_project_Main_topLevelTailFunc,
                                      &literal_int_0))),
                      }))),
          })));
}
Closure author_project_Main_view = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_author_project_Main_view,
};
#define author_project_Main_main (*ptr_author_project_Main_main)
ElmValue* ptr_author_project_Main_main;
void* init_author_project_Main_main() {
  return A1(&elm_browser_Browser_sandbox,
      NEW_RECORD(&fg_init_update_view,
          3,
          ((void* []){
              &author_project_Main_initialModel,
              &author_project_Main_update,
              &author_project_Main_view,
          })));
}

void** mains[] = {
    &ptr_author_project_Main_main,
    NULL,
};

int EMSCRIPTEN_KEEPALIVE main() {
  int exit_code = GC_init();
  if (exit_code) return exit_code;
  Utils_initGlobal(&ptr_elm_core_Platform_Cmd_none, &init_elm_core_Platform_Cmd_none);
  Utils_initGlobal(&ptr_elm_core_Platform_Sub_none, &init_elm_core_Platform_Sub_none);
  Utils_initGlobal(&ptr_elm_html_Html_div, &init_elm_html_Html_div);
  Utils_initGlobal(&ptr_author_project_Main_main, &init_author_project_Main_main);
  Wrapper_registerFieldGroups(app_field_groups);
  Wrapper_registerMains(mains);
  return 0;
}
