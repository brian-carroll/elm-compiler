#include "kernel.h"
enum {
  CTOR_Generate,
  CTOR_Generator,
  CTOR_NewValue,
  CTOR_Normal,
  CTOR_Seed,
};
enum {
  JS_Bitwise_and,
  JS_Bitwise_shiftRightZfBy,
  JS_Bitwise_xor,
  JS_Browser_element,
  JS_Json_succeed,
  JS_Platform_batch,
  JS_Platform_leaf,
  JS_VirtualDom_node,
  JS_VirtualDom_on,
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
Closure VirtualDom_on = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_VirtualDom_on,
};
Closure VirtualDom_node = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_VirtualDom_node,
};
Closure Platform_leaf = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Platform_leaf,
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
Closure Bitwise_xor = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Bitwise_xor,
};
Closure Bitwise_shiftRightZfBy = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Bitwise_shiftRightZfBy,
};
Closure Bitwise_and = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Bitwise_and,
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
ElmString16 literal_string_click = {
    .header = HEADER_STRING(5),
    .words16 =
        {
            0x63,
            0x6c,
            0x69,
            0x63,
            0x6b,
        },
};
ElmString16 literal_string_button = {
    .header = HEADER_STRING(6),
    .words16 =
        {
            0x62,
            0x75,
            0x74,
            0x74,
            0x6f,
            0x6e,
        },
};
ElmString16 literal_string_Random = {
    .header = HEADER_STRING(6),
    .words16 =
        {
            0x52,
            0x61,
            0x6e,
            0x64,
            0x6f,
            0x6d,
        },
};
ElmString16 literal_string_Generate_21 = {
    .header = HEADER_STRING(9),
    .words16 =
        {
            0x47,
            0x65,
            0x6e,
            0x65,
            0x72,
            0x61,
            0x74,
            0x65,
            0x21,
        },
};
ElmInt literal_int_277803737 = {
    .header = HEADER_INT,
    .value = 277803737,
};
ElmInt literal_int_1664525 = {
    .header = HEADER_INT,
    .value = 1664525,
};
ElmInt literal_int_28 = {
    .header = HEADER_INT,
    .value = 28,
};
ElmInt literal_int_22 = {
    .header = HEADER_INT,
    .value = 22,
};
ElmInt literal_int_4 = {
    .header = HEADER_INT,
    .value = 4,
};
ElmInt literal_int_1 = {
    .header = HEADER_INT,
    .value = 1,
};
ElmInt literal_int_0 = {
    .header = HEADER_INT,
    .value = 0,
};
FieldGroup* app_field_groups[] = {
    &fg_init_subscriptions_update_view,
    NULL,
};

void* eval_elm_core_Basics_always(void* args[]) {
  void* x_a = args[0];
  void* x__v0 = args[1];
  return x_a;
}
Closure elm_core_Basics_always = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_core_Basics_always,
};

#define elm_browser_Browser_element Browser_element

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

#define elm_json_Json_Decode_succeed Json_succeed

void* eval_author_project_Main_NewValue(void* args[]) {
  return ctorCustom(CTOR_NewValue, 1, args);
}
Closure author_project_Main_NewValue = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_author_project_Main_NewValue,
};

void* eval_elm_random_Random_Generate(void* args[]) {
  return ctorCustom(CTOR_Generate, 1, args);
}
Closure elm_random_Random_Generate = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_random_Random_Generate,
};

Closure elm_random_Random_command = {
    .header = HEADER_CLOSURE(1),
    .n_values = 0x1,
    .max_values = 0xffff,
    .evaluator = (void*)JS_Platform_leaf,
    .values =
        {
            &literal_string_Random,
        },
};

void* eval_elm_core_Basics_identity(void* args[]) {
  void* x_x = args[0];
  return x_x;
}
Closure elm_core_Basics_identity = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_core_Basics_identity,
};

void* eval_elm_random_Random_Generator(void* args[]) {
  return ctorCustom(CTOR_Generator, 1, args);
}
Closure elm_random_Random_Generator = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_random_Random_Generator,
};
void* eval_elm_random_Random_map_lambda0(void* args[]) {
  void* x_func = args[0];
  void* x_genA = args[1];
  void* x_seed0 = args[2];
  void* x__v1 = A1(x_genA, x_seed0);
  void* x_a = Utils_destruct_index(x__v1, 0);
  void* x_seed1 = Utils_destruct_index(x__v1, 1);
  return NEW_TUPLE2(A1(x_func, x_a), x_seed1);
}
void* eval_elm_random_Random_map(void* args[]) {
  void* x_func = args[0];
  void* x__v0 = args[1];
  void* x_genA = ((Custom*)x__v0)->values[0];
  return A1(&elm_random_Random_Generator,
      NEW_CLOSURE(2,
          3,
          &eval_elm_random_Random_map_lambda0,
          ((void* []){
              x_func,
              x_genA,
          })));
}
Closure elm_random_Random_map = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_random_Random_map,
};
void* eval_elm_random_Random_generate(void* args[]) {
  void* x_tagger = args[0];
  void* x_generator = args[1];
  return A1(&elm_random_Random_command,
      A1(&elm_random_Random_Generate, A2(&elm_random_Random_map, x_tagger, x_generator)));
}
Closure elm_random_Random_generate = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_random_Random_generate,
};

void* eval_elm_random_Random_andThen_lambda0(void* args[]) {
  void* x_callback = args[0];
  void* x_genA = args[1];
  void* x_seed = args[2];
  void* x__v1 = A1(x_genA, x_seed);
  void* x_result = Utils_destruct_index(x__v1, 0);
  void* x_newSeed = Utils_destruct_index(x__v1, 1);
  void* x__v2 = A1(x_callback, x_result);
  void* x_genB = ((Custom*)x__v2)->values[0];
  return A1(x_genB, x_newSeed);
}
void* eval_elm_random_Random_andThen(void* args[]) {
  void* x_callback = args[0];
  void* x__v0 = args[1];
  void* x_genA = ((Custom*)x__v0)->values[0];
  return A1(&elm_random_Random_Generator,
      NEW_CLOSURE(2,
          3,
          &eval_elm_random_Random_andThen_lambda0,
          ((void* []){
              x_callback,
              x_genA,
          })));
}
Closure elm_random_Random_andThen = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_random_Random_andThen,
};

void* eval_elm_core_Basics_apR(void* args[]) {
  void* x_x = args[0];
  void* x_f = args[1];
  return A1(x_f, x_x);
}
Closure elm_core_Basics_apR = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_core_Basics_apR,
};

#define elm_core_Basics_eq Utils_equal

#define elm_core_Basics_add Basics_add

#define elm_core_Bitwise_and Bitwise_and

#define elm_core_Basics_lt Utils_lt

void* eval_elm_core_Basics_negate(void* args[]) {
  void* x_n = args[0];
  return A1(&elm_core_Basics_negate, x_n);
}
Closure elm_core_Basics_negate = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_core_Basics_negate,
};

void* eval_elm_random_Random_Seed(void* args[]) {
  return ctorCustom(CTOR_Seed, 2, args);
}
Closure elm_random_Random_Seed = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_random_Random_Seed,
};

#define elm_core_Basics_mul Basics_mul

#define elm_core_Bitwise_shiftRightZfBy Bitwise_shiftRightZfBy
void* eval_elm_random_Random_next(void* args[]) {
  void* x__v0 = args[0];
  void* x_state0 = Utils_destruct_index(x__v0, 0);
  void* x_incr = Utils_destruct_index(x__v0, 1);
  return A2(&elm_random_Random_Seed,
      A2(&elm_core_Bitwise_shiftRightZfBy,
          &literal_int_0,
          A2(&elm_core_Basics_add,
              A2(&elm_core_Basics_mul, x_state0, &literal_int_1664525),
              x_incr)),
      x_incr);
}
Closure elm_random_Random_next = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_random_Random_next,
};

#define elm_core_Bitwise_xor Bitwise_xor
void* eval_elm_random_Random_peel(void* args[]) {
  void* x__v0 = args[0];
  void* x_state = Utils_destruct_index(x__v0, 0);
  void* x_word = A2(&elm_core_Basics_mul,
      A2(&elm_core_Bitwise_xor,
          x_state,
          A2(&elm_core_Bitwise_shiftRightZfBy,
              A2(&elm_core_Basics_add,
                  A2(&elm_core_Bitwise_shiftRightZfBy, &literal_int_28, x_state),
                  &literal_int_4),
              x_state)),
      &literal_int_277803737);
  return A2(&elm_core_Bitwise_shiftRightZfBy,
      &literal_int_0,
      A2(&elm_core_Bitwise_xor,
          A2(&elm_core_Bitwise_shiftRightZfBy, &literal_int_22, x_word),
          x_word));
}
Closure elm_random_Random_peel = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_random_Random_peel,
};

#define elm_core_Basics_remainderBy Basics_remainderBy

#define elm_core_Basics_sub Basics_sub
void* eval_elm_random_Random_int_lambda0(void* args[]) {
  void* x_a = args[0];
  void* x_accountForBias = args[1];
  void* x_b = args[2];
  void* x_seed0 = args[3];
  void* tmp1;
  if (A2(&elm_core_Basics_lt, x_a, x_b) == &True) {
    tmp1 = NEW_TUPLE2(x_a, x_b);
  } else {
    tmp1 = NEW_TUPLE2(x_b, x_a);
  };
  void* x__v0 = tmp1;
  void* x_lo = Utils_destruct_index(x__v0, 0);
  void* x_hi = Utils_destruct_index(x__v0, 1);
  void* x_range =
      A2(&elm_core_Basics_add, A2(&elm_core_Basics_sub, x_hi, x_lo), &literal_int_1);
  void* tmp2;
  if (A2(&elm_core_Basics_eq,
          A2(&elm_core_Bitwise_and,
              A2(&elm_core_Basics_sub, x_range, &literal_int_1),
              x_range),
          &literal_int_0) == &True) {
    tmp2 = NEW_TUPLE2(A2(&elm_core_Basics_add,
                          A2(&elm_core_Bitwise_shiftRightZfBy,
                              &literal_int_0,
                              A2(&elm_core_Bitwise_and,
                                  A2(&elm_core_Basics_sub, x_range, &literal_int_1),
                                  A1(&elm_random_Random_peel, x_seed0))),
                          x_lo),
        A1(&elm_random_Random_next, x_seed0));
  } else {
    void* x_threshhold = A2(&elm_core_Bitwise_shiftRightZfBy,
        &literal_int_0,
        A2(&elm_core_Basics_remainderBy,
            x_range,
            A2(&elm_core_Bitwise_shiftRightZfBy,
                &literal_int_0,
                A1(&elm_core_Basics_negate, x_range))));
    // accountForBias;
    tmp2 = A1(x_accountForBias, x_seed0);
  };
  return tmp2;
}
void* eval_elm_random_Random_int(void* args[]) {
  void* x_accountForBias = args[0];
  void* x_a = args[1];
  void* x_b = args[2];
  return A1(&elm_random_Random_Generator,
      NEW_CLOSURE(3,
          4,
          &eval_elm_random_Random_int_lambda0,
          ((void* []){
              x_a,
              x_accountForBias,
              x_b,
          })));
}
Closure elm_random_Random_int = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_random_Random_int,
};

//
void* cycle_author_project_Main_rollAgain();
void* cycle_author_project_Main_roll();

void* eval_author_project_Main_rollIf(void* args[]) {
  void* x_b = args[0];
  void* tmp0;
  if (x_b == &True) {
    tmp0 = A2(&elm_random_Random_int, &literal_int_1, &literal_int_6);
  } else {
    tmp0 = cycle_author_project_Main_rollAgain();
  };
  return tmp0;
}
Closure author_project_Main_rollIf = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_author_project_Main_rollIf,
};

#define author_project_Main_rollAgain (*ptr_author_project_Main_rollAgain)
void* ptr_author_project_Main_rollAgain;
void* init_author_project_Main_rollAgain() {
  return cycle_author_project_Main_rollAgain();
}
void* cycle_author_project_Main_rollAgain() {
  if (!ptr_author_project_Main_rollAgain) {
    ptr_author_project_Main_rollAgain = cycle_author_project_Main_roll();
  }
  return ptr_author_project_Main_rollAgain;
}

#define author_project_Main_roll (*ptr_author_project_Main_roll)
void* ptr_author_project_Main_roll;
void* init_author_project_Main_roll() {
  return cycle_author_project_Main_roll();
}
void* eval_author_project_Main_roll_lambda0(void* args[]) {
  void* x_n = args[0];
  return Utils_equal(x_n, &literal_int_1);
}
void* cycle_author_project_Main_roll() {
  if (ptr_author_project_Main_roll) return ptr_author_project_Main_roll;
  ptr_author_project_Main_roll = A2(&elm_random_Random_andThen,
      &author_project_Main_rollIf,
      A2(&elm_random_Random_map,
          NEW_CLOSURE(0, 1, &eval_author_project_Main_roll_lambda0, ((void* []){})),
          A2(&elm_random_Random_int, &literal_int_0, &literal_int_1)));
  return ptr_author_project_Main_roll;
}

// ---------------------------------------------------------------

void* eval_author_project_Main_update(void* args[]) {
  void* x_msg = args[0];
  void* x_model = args[1];
  void* tmp0;
  void* tmp1 = ((Custom*)x_msg)->ctor;
  if (tmp1 == CTOR_Generate) {
    tmp0 = NEW_TUPLE2(x_model,
        A2(&elm_random_Random_generate,
            &author_project_Main_NewValue,
            &author_project_Main_roll));
  } else {
    void* x_v = Utils_destruct_index(x_msg, 0);
    tmp0 = NEW_TUPLE2(x_v, &elm_core_Platform_Cmd_none);
  };
  return tmp0;
}
Closure author_project_Main_update = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_author_project_Main_update,
};

Custom author_project_Main_Generate = {
    .header = HEADER_CUSTOM(0),
    .ctor = CTOR_Generate,
};

#define elm_html_Html_button (*ptr_elm_html_Html_button)
ElmValue* ptr_elm_html_Html_button;
void* init_elm_html_Html_button() {
  return A1(&VirtualDom_node, &literal_string_button);
}

#define elm_html_Html_div (*ptr_elm_html_Html_div)
ElmValue* ptr_elm_html_Html_div;
void* init_elm_html_Html_div() {
  return A1(&VirtualDom_node, &literal_string_div);
}

#define elm_core_String_fromInt String_fromNumber

void* eval_elm_virtual_dom_VirtualDom_Normal(void* args[]) {
  return ctorCustom(CTOR_Normal, 1, args);
}
Closure elm_virtual_dom_VirtualDom_Normal = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_virtual_dom_VirtualDom_Normal,
};

#define elm_virtual_dom_VirtualDom_on VirtualDom_on
void* eval_elm_html_Html_Events_on(void* args[]) {
  void* x_event = args[0];
  void* x_decoder = args[1];
  return A2(&elm_virtual_dom_VirtualDom_on,
      x_event,
      A1(&elm_virtual_dom_VirtualDom_Normal, x_decoder));
}
Closure elm_html_Html_Events_on = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x2,
    .evaluator = &eval_elm_html_Html_Events_on,
};
void* eval_elm_html_Html_Events_onClick(void* args[]) {
  void* x_msg = args[0];
  return A2(&elm_html_Html_Events_on,
      &literal_string_click,
      A1(&elm_json_Json_Decode_succeed, x_msg));
}
Closure elm_html_Html_Events_onClick = {
    .header = HEADER_CLOSURE(0),
    .n_values = 0x0,
    .max_values = 0x1,
    .evaluator = &eval_elm_html_Html_Events_onClick,
};

#define elm_virtual_dom_VirtualDom_text VirtualDom_text
#define elm_html_Html_text elm_virtual_dom_VirtualDom_text
void* eval_author_project_Main_view(void* args[]) {
  void* x_model = args[0];
  return A2(&elm_html_Html_div,
      &Nil,
      List_fromArray(2,
          ((void* []){
              A1(&elm_html_Html_text, A1(&elm_core_String_fromInt, x_model)),
              A2(&elm_html_Html_button,
                  List_fromArray(1,
                      ((void* []){
                          A1(&elm_html_Html_Events_onClick,
                              &author_project_Main_Generate),
                      })),
                  List_fromArray(1,
                      ((void* []){
                          A1(&elm_html_Html_text, &literal_string_Generate_21),
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
void* eval_author_project_Main_main_lambda0(void* args[]) {
  void* x__v0 = args[0];
  return NEW_TUPLE2(&literal_int_0, &elm_core_Platform_Cmd_none);
}
void* init_author_project_Main_main() {
  return A1(&elm_browser_Browser_element,
      NEW_RECORD(&fg_init_subscriptions_update_view,
          4,
          ((void* []){
              NEW_CLOSURE(0, 1, &eval_author_project_Main_main_lambda0, ((void* []){})),
              A1(&elm_core_Basics_always, &elm_core_Platform_Sub_none),
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
  Utils_initGlobal(&ptr_elm_html_Html_button, &init_elm_html_Html_button);
  Utils_initGlobal(&ptr_elm_html_Html_div, &init_elm_html_Html_div);
  Utils_initGlobal(&ptr_author_project_Main_main, &init_author_project_Main_main);
  Wrapper_registerFieldGroups(app_field_groups);
  Wrapper_registerMains(mains);
  return 0;
}
