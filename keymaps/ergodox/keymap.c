#include "ergodox.h"
#include "debug.h"
#include "action_layer.h"
#include "version.h"

#define BASE 0 // default layer
#define SYMB 1 // symbols
#define MDIA 2 // media keys

enum layers {
    BASE,   // default layer
    SYMB,   // symbols and macros
    CPP,    // C/C++ macros
    MEDIA,  // media, numpad, mouse buttons
    DVORAK,
    COLEMAK,
};

enum macros {
    // Dvorak
    D_0,
    D_1,
    D_2,
    D_3,
    D_4,
    D_5,
    D_6,
    D_7,
    D_8,
    D_9,

    // Generic macros (vim, tmux, etc)
    DOUBLE_ZERO,
    
    // C++ macros
    INC,
    DEC,
    EQ,
    NEQ,
    GTE,
    LTE,
    LAND,
    LOR,
    SHR,
    SHL,
    ADD_EQ,
    SUB_EQ,
    MUL_EQ,
    DIV_EQ,
    MOD_EQ,
    AND_EQ,
    OR_EQ,
    XOR_EQ,
    SHR_EQ,
    SHL_EQ,
    DEREF,
};

enum custom_keycodes {
  PLACEHOLDER = SAFE_RANGE, // can always be here
  EPRM,
  VRSN,
  TMUX,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
/* Keymap 0: Basic layer
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |  Grv   |   1  |   2  |   3  |   4  |   5  | Esc  |           |  Esc |   6  |   7  |   8  |   9  |   0  |   -    |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |  Tab   |   Q  |   W  |   E  |   R  |   T  |  L1  |           |  L1  |   Y  |   U  |   I  |   O  |   P  |   \    |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |  Ctrl  |   A  |   S  |   D  |   F  |   G  |------|           |------|   H  |   J  |   K  |   L  |   ;  |   '    |
 * |--------+------+------+------+------+------| Hyper|           | Meh  |------+------+------+------+------+--------|
 * | LShift |   Z  |   X  |   C  |   V  |   B  |      |           |      |   N  |   M  |   ,  |   .  |   /  |RShift/=|
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |  f3  | Meta | LAlt |  f2  |  f1  |                                       | LEAD | PgUp | PgDn | Home |  End |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      | Home |       | PgUp |      |      |
 *                                 | Bksp | Del  |------|       |------| Enter| Space|
 *                                 |      |      | End  |       | PgDn |      |      |
 *                                 `--------------------'       `--------------------'
 */
[BASE] = KEYMAP(  // layer 0 : default
        // left hand
        KC_GRV,     KC_1,       KC_2,       KC_3,       KC_4,   KC_5,   KC_ESC,
        KC_TAB,     KC_Q,       KC_W,       KC_E,       KC_R,   KC_T,   TG(SYMB),
        KC_LCTRL,   KC_A,       KC_S,       KC_D,       KC_F,   KC_G,
        KC_LSFT,    KC_Z,       KC_X,       KC_C,       KC_V,   KC_B,   ALL_T(KC_NO),
        TT(SYMB),   KC_LGUI,    KC_LALT,    TT(MEDIA),  OSL(CPP),
                                                        KC_NO,          KC_NO,
                                                                        KC_HOME,
                                            KC_BSPC,    KC_DEL,         KC_END,
        // right hand
             KC_ESC,      KC_6,   KC_7,     KC_8,       KC_9,   KC_0,       KC_MINS,
             TG(SYMB),    KC_Y,   KC_U,     KC_I,       KC_O,   KC_P,       KC_BSLS,
                          KC_H,   KC_J,     KC_K,       KC_L,   KC_SCLN,    KC_QUOT,
             MEH_T(KC_NO),KC_N,   KC_M,     KC_COMM,    KC_DOT, KC_SLSH,    MT(MOD_RSFT,KC_EQ),
                                  KC_LEAD,  KC_PGUP,    KC_PGDN,KC_HOME,    KC_END,
             KC_NO,          KC_NO,        
             KC_PGUP,
             KC_PGDN,KC_ENT, KC_SPC
    ),
/* Keymap 1: Symbol Layer
 *
 * ,---------------------------------------------------.           ,--------------------------------------------------.
 * |         |  F1  |  F2  |  F3  |  F4  |  F5  | F11  |           |  F12 |  F6  |  F7  |  F8  |  F9  |  F10 | NumLk  |
 * |---------+------+------+------+------+------+------|           |------+------+------+------+------+------+--------|
 * |         |   !  |   @  |   {  |   }  |   |  |      |           |      |      |   7  |   8  |   9  |   /  |        |
 * |---------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |         |   #  |   $  |   (  |   )  |   &  |------|           |------|      |   4  |   5  |   6  |   *  | Enter  |
 * |---------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |         |   %  |   ^  |   [  |   ]  |   =  |      |           |      |      |   1  |   2  |   3  |   -  |        |
 * `---------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |       |      |      |      |      |                                       |   00 |   0  |   .  |   +  |      |
 *   `-----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |      |
 *                                 |      |      |------|       |------|      |      |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
// SYMBOLS
[SYMB] = KEYMAP(
       // left hand
       KC_TRNS,KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,  KC_TRNS,
       KC_TRNS,KC_EXLM,KC_AT,  KC_LCBR,KC_RCBR,KC_PIPE,KC_TRNS,
       KC_TRNS,KC_HASH,KC_DLR, KC_LPRN,KC_RPRN,KC_GRV,
       KC_TRNS,KC_PERC,KC_CIRC,KC_LBRC,KC_RBRC,KC_TILD,KC_TRNS,
       KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,
                                       KC_TRNS,KC_TRNS,
                                               KC_TRNS,
                               KC_TRNS,KC_TRNS,KC_TRNS,
       // right hand
       KC_F12,  KC_F6,   KC_F7,         KC_F8,      KC_F9,      KC_F10,     KC_NUMLOCK,
       KC_TRNS, KC_TRNS, KC_KP_7,       KC_KP_8,    KC_KP_9,    KC_PSLS,    KC_TRNS,
                KC_TRNS, KC_KP_4,       KC_KP_5,    KC_KP_6,    KC_PAST,    KC_PENT,
       KC_TRNS, KC_TRNS, KC_KP_1,       KC_KP_2,    KC_KP_3,    KC_PMNS,    KC_TRNS,
                         M(DOUBLE_ZERO),KC_KP_0,    KC_KP_DOT,  KC_PPLS,    KC_TRNS,
       KC_TRNS, KC_TRNS,
       KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS
),
/* Keymap 3: C/C++ Macros
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |        |      |      |      |      |      |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |  >>  |  !=  |  <=  |  >=  |      |      |           |      |      |      |  &=  |  |=  |  ^=  |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |  ->  |  ==  |  &&  |  ||  |  ::  |------|           |------|      |  +=  |  -=  |  *=  |  /=  |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |  <<  |  --  |  ++  | ...  |      |      |           |      |      |      |  %=  |  <<= |  >>= |        |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |      |      |      |                                       |      |      |      |      |      |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |      |
 *                                 |      |      |------|       |------|      |      |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
// CPP MACROS
[MDIA] = KEYMAP(
       KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, M(SHR),  M(NEQ),  M(LTE),  M(GTE),  KC_TRNS, KC_TRNS,
       KC_TRNS, M(DEREF),M(EQ),   M(LAND), M(LOR),  M(SCOPE),
       KC_TRNS, M(SHL),  M(DEC),  M(INC),  M(ELIPS),KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS, KC_BTN1, KC_BTN2,
                                           KC_TRNS, KC_TRNS,
                                                    KC_TRNS,
                                  KC_TRNS, KC_TRNS, KC_TRNS,
    // right hand
       KC_TRNS,  KC_TRNS,   KC_TRNS,   KC_TRNS,   KC_TRNS,   KC_TRNS,   KC_TRNS,
       KC_TRNS,  KC_TRNS,   KC_TRNS,   M(AND_EQ), M(OR_EQ),  M(XOR_EQ), KC_TRNS,
                 KC_TRNS,   M(ADD_EQ), M(SUB_EQ), M(MUL_EQ), M(DIV_EQ), KC_TRNS,
       KC_TRNS,  KC_TRNS,   KC_TRNS,   M(MOD_EQ), M(SHL_EQ), M(SHR_EQ), KC_TRNS,
                            KC_TRNS,   KC_TRNS,   KC_TRNS,   KC_TRNS,   KC_TRNS,
       KC_TRNS, KC_TRNS,
       KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS
),
/* Keymap 2: Media and mouse keys
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |        |      |      |      |      |      |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |      |      | MsUp |      |      |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |MsLeft|MsDown|MsRght|      |------|           |------|      |      |      |      |      |  Play  |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |      |      |      |      |      |           |      |      |      | Prev | Next |      |        |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |      | Lclk | Rclk |                                       |VolUp |VolDn | Mute |      |      |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |Brwser|
 *                                 |      |      |------|       |------|      |Back  |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
// MEDIA AND MOUSE
[MEDIA] = KEYMAP(
        // left hand
       KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS, KC_MS_U, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_MS_L, KC_MS_D, KC_MS_R, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS, KC_BTN1, KC_BTN2,
                                           KC_TRNS, KC_TRNS,
                                                    KC_TRNS,
                                  KC_TRNS, KC_TRNS, KC_TRNS,
    // right hand
       KC_TRNS,  KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS,  KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
                 KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_MPLY,
       KC_TRNS,  KC_TRNS, KC_TRNS, KC_MPRV, KC_MNXT, KC_TRNS, KC_TRNS,
                          KC_VOLU, KC_VOLD, KC_MUTE, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS,
       KC_TRNS,
       KC_TRNS, KC_TRNS, KC_WBAK
),
[DVORAK] = KEYMAP(
        // left hand
        F(D_GRV),   F(D_9),     F(D_7),     F(D_5),     F(D_3),   F(D_1), KC_ESC,
        KC_TAB,     KC_SCLN,    KC_COMM,    KC_DOT,     KC_P,     KC_Y,   TG(SYMB),
        KC_LCTRL,   KC_A,       KC_O,       KC_E,       KC_U,     KC_I,
        KC_LSFT,    KC_QUOT,    KC_Q,       KC_J,       KC_K,     KC_X,   ALL_T(KC_NO),
        TT(SYMB),   KC_LGUI,    KC_LALT,    TT(MEDIA),  OSL(CPP),
                                                        LGUI(KC_M),     KC_NO,
                                                                        KC_HOME,
                                            KC_BSPC,    KC_DEL,         KC_END,
        // right hand
        KC_ESC,      F(D_0), F(D_2),    F(D_4),     F(D_6),   F(D_8),   F(D_EQ),
        TG(SYMB),    KC_F,   KC_G,      KC_C,       KC_R,     KC_L,     KC_BSLS,
                     KC_D,   KC_H,      KC_T,       KC_N,     KC_S,     KC_MINS,
        MEH_T(KC_NO),KC_B,   KC_M,      KC_W,       KC_V,     KC_Z,     MT(MOD_RSFT,KC_EQ),
                             KC_LEAD,   KC_PGUP,    KC_PGDN,  KC_HOME,  KC_END,
        KC_NO,       CTL_T(KC_ESC),
        KC_PGUP,
        KC_PGDN,KC_ENT, KC_SPC
    ),
[COLEMAK] = KEYMAP(  // layer 0 : default
        // left hand
        KC_GRV,     KC_1,       KC_2,       KC_3,       KC_4,     KC_5,   KC_ESC,
        KC_TAB,     KC_Q,       KC_W,       KC_F,       KC_P,     KC_G,   TG(SYMB),
        KC_LCTRL,   KC_A,       KC_R,       KC_S,       KC_T,     KC_D,
        KC_LSFT,    KC_Z,       KC_X,       KC_C,       KC_V,     KC_B,   ALL_T(KC_NO),
        TT(SYMB),   KC_LGUI,    KC_LALT,    TT(MEDIA),  OSL(CPP),
                                                        LGUI(KC_M),     KC_NO,
                                                                        KC_HOME,
                                            KC_BSPC,    KC_DEL,         KC_END,
        // right hand
             KC_ESC,      KC_6,   KC_7,     KC _8,      KC_9,     KC_0,      KC_MINS,
             TG(SYMB),    KC_J,   KC_L,     KC_U,       KC_Y,     KC_SCLN,   KC_BSLS,
                          KC_H,   KC_N,     KC_E,       KC_I,     KC_O,      KC_QUOT,
             MEH_T(KC_NO),KC_K,   KC_M,     KC_COMM,    KC_DOT,   KC_SLSH,   MT(MOD_RSFT,KC_EQ),
                                  KC_LEAD,  KC_PGUP,    KC_PGDN,  KC_HOME,   KC_END,
             KC_NO,          CTL_T(KC_ESC),
             KC_PGUP,
             KC_PGDN,KC_ENT, KC_SPC
    ),
};

const uint16_t PROGMEM fn_actions[] = {
    [1] = ACTION_LAYER_TAP_TOGGLE(SYMB)                // FN1 - Momentary Layer 1 (Symbols)
};

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt)
{
  // MACRODOWN only works in this function
    switch(id) {
        // Dvorak
        case D_0:
            if (record->event.pressed) {

            }
            break;

        case D_1:
        case D_2:
        case D_3:
        case D_4:
        case D_5:
        case D_6:
        case D_7:
        case D_8:
        case D_9:

        // C/C++ macros
        case INC:     if (record->event.pressed) SEND_STRING("++");     break;
        case DEC:     if (record->event.pressed) SEND_STRING("--");     break;
        case EQ:      if (record->event.pressed) SEND_STRING("==");     break;
        case NEQ:     if (record->event.pressed) SEND_STRING("!=");     break;
        case GTE:     if (record->event.pressed) SEND_STRING(">=");     break;
        case LTE:     if (record->event.pressed) SEND_STRING("<=");     break;
        case LAND:    if (record->event.pressed) SEND_STRING("&&");     break;
        case LOR:     if (record->event.pressed) SEND_STRING("||");     break;
        case SHR:     if (record->event.pressed) SEND_STRING(">>");     break;
        case SHL:     if (record->event.pressed) SEND_STRING("<<");     break;
        case ADD_EQ:  if (record->event.pressed) SEND_STRING("+=");     break;
        case SUB_EQ:  if (record->event.pressed) SEND_STRING("-=");     break;
        case MUL_EQ:  if (record->event.pressed) SEND_STRING("*=");     break;
        case DIV_EQ:  if (record->event.pressed) SEND_STRING("/=");     break;
        case MOD_EQ:  if (record->event.pressed) SEND_STRING("%=");     break;
        case AND_EQ:  if (record->event.pressed) SEND_STRING("&=");     break;
        case OR_EQ:   if (record->event.pressed) SEND_STRING("|=");     break;
        case XOR_EQ:  if (record->event.pressed) SEND_STRING("^=");     break;
        case SHR_EQ:  if (record->event.pressed) SEND_STRING(">>=");    break;
        case SHL_EQ:  if (record->event.pressed) SEND_STRING("<<=");    break;
        case DEREF:   if (record->event.pressed) SEND_STRING("->");     break;
    }
    return MACRO_NONE;
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  return true;
}

// Runs just one time when the keyboard initializes.
void matrix_init_user(void) {};


// Runs constantly in the background, in a loop.
void matrix_scan_user(void) {

    uint8_t layer = biton32(layer_state);

    ergodox_board_led_off();
    ergodox_right_led_1_off();
    ergodox_right_led_2_off();
    ergodox_right_led_3_off();
    switch (layer) {
      // TODO: Make this relevant to the ErgoDox EZ.
        case 1:
            ergodox_right_led_1_on();
            break;
        case 2:
            ergodox_right_led_2_on();
            break;
        default:
            // none
            break;
    }

};
