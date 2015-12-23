#define _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_DEPRECATE
#define NOMINMAX
#include <ctime>
#include <string>
#include <cstring>
#include <vector>
#include "xgboost/src/sync/sync.h"
#include "xgboost/src/io/io.h"
#include "xgboost/src/utils/utils.h"
#include "xgboost/src/utils/config.h"
#include "xgboost/src/learner/learner-inl.hpp"
#include "xgboost/src/io/simple_dmatrix-inl.hpp"
#include "xgboost/src/io/page_dmatrix-inl.hpp"

#include "exgb_commands.h"

#include "erl_driver.h"
#include "erl_interface.h"
#include "ei.h"
#include "assert.h"

#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
#define ErlDrvSizeT int
#define ErlDrvSSizeT int
#endif

#define MAX_MODELS 4096

// State structure
typedef struct {
  long model_idx;
  xgboost::learner::BoostLearner *models[MAX_MODELS];
  ErlDrvPort drv_port;
} state;

ei_x_buff *badarg();
ei_x_buff *do_predict(state *st, char *args, int index);
ei_x_buff *do_load_model(state *st, char *args, int index);

// =============================================================================
// Erlang Callbacks
// =============================================================================

extern "C" {
    static ErlDrvData start(ErlDrvPort port, char *command) {
      state *drvstate = (state *)driver_alloc(sizeof(state));
      drvstate->drv_port = port;
      drvstate->model_idx = 0;
      return (ErlDrvData)drvstate;
    }

    static void stop(ErlDrvData drvstate) {
      state *st = (state *)drvstate;
      driver_select(st->drv_port, (ErlDrvEvent)(size_t)fileno(stdin), DO_READ, 0);
      driver_free(drvstate);
    }

    static void output(ErlDrvData drvstate, char *buff,
                       ErlDrvSizeT bufflen)
    {
        state *st = (state *)drvstate;
        long fn;
        int index = 0, version, arity;
        ei_x_buff *ret = NULL;

        ei_decode_version(buff, &index, &version);

        ei_decode_tuple_header(buff, &index, &arity);
        ei_decode_long(buff, &index, &fn);
        //printf("fn: %ld a: %d idx: %d\n\r", fn, arity, index);

        switch (fn) {
        case PREDICT:    ret = do_predict(st, buff, index); break;
        case LOAD_MODEL: ret = do_load_model(st, buff, index); break;
        default:         ret = badarg(); break;
        }

        driver_output(st->drv_port, ret->buff, ret->index);
    }
}

// =============================================================================
// XGBoost function wrappers
// ===========================================================================

ei_x_buff *do_predict(state *st, char *args, int index) {
    int pred_margin = 0;
    int ntree_limit = 0;
    int arity;
    long model;
    ei_decode_tuple_header(args, &index, &arity);
    ei_decode_long(args, &index, &model);
    xgboost::learner::BoostLearner *learner = st->models[model];
    ei_decode_list_header(args, &index, &arity);
    if (arity > 0) {
        std::vector<float> preds;
        xgboost::io::DMatrixSimple *data = new xgboost::io::DMatrixSimple();

        std::vector<xgboost::RowBatch::Entry> feats;
        int nfet = arity;
        feats.resize(nfet);
        for (int i = 0; i < nfet; i++) {
            double feat_i;
            ei_decode_double(args, &index, &feat_i);
            feats[i] = xgboost::RowBatch::Entry(i, feat_i);
        }
        data->AddRow(feats);
        learner->Predict(*data, pred_margin != 0, &preds, ntree_limit);

        ei_x_buff *x = (ei_x_buff *) malloc(sizeof(ei_x_buff));
        ei_x_new_with_version(x);
        ei_x_encode_tuple_header(x, 2);
        ei_x_encode_atom(x, "ok");
        ei_x_encode_double(x, preds[0]);
        return x;
    } else {
        return badarg();
    }
}

ei_x_buff *do_load_model(state *st, char *args, int index) {
    long strlen;
    int arity;

    ei_decode_tuple_header(args, &index, &arity);
    assert(arity == 2);
    ei_decode_long(args, &index, &strlen);
    //printf("strlen: %ld a: %d idx: %d\n", strlen, arity, index);
    char model[strlen];
    ei_decode_string(args, &index, model);

    ei_x_buff *x = (ei_x_buff *) malloc(sizeof(ei_x_buff));
    ei_x_new_with_version(x);

    try {
        xgboost::learner::BoostLearner *learner = (xgboost::learner::BoostLearner *)malloc(sizeof(xgboost::learner::BoostLearner));
        learner->LoadModel(model);

        st->models[st->model_idx] = learner;

        ei_x_encode_tuple_header(x, 2);
        ei_x_encode_atom(x, "ok");
        ei_x_encode_long(x, st->model_idx);

        st->model_idx++;
    } catch (std::exception& load_error) {
        // TODO: fix me - actuall catch and return the error tuple
        ei_x_encode_tuple_header(x, 2);
        ei_x_encode_atom(x, "error");
        ei_x_encode_long(x, -1L);
    }

    return x;
}



// =============================================================================
// Utility functions
// =============================================================================

ei_x_buff *badarg() {
    ei_x_buff *ret = (ei_x_buff *) malloc(sizeof(ei_x_buff));
    ei_x_new_with_version(ret);
    ei_x_encode_atom(ret, "badarg");
    return ret;
}

// =============================================================================
// Erlang driver_entry Specification
// ===========================================================================

extern "C" {
ErlDrvEntry driver_entry = {
    NULL,           /* F_PTR init, called when driver is loaded */
    start,      /* L_PTR start, called when port is opened */
    stop,       /* F_PTR stop, called when port is closed */
    output,       /* F_PTR output, called when erlang has sent */
    NULL,           /* F_PTR ready_input, called when input descriptor ready */
    NULL,           /* F_PTR ready_output, called when output descriptor ready */
    "exgb",      /* char *driver_name, the argument to open_port */
    NULL,           /* F_PTR finish, called when unloaded */
    NULL,                       /* void *handle, Reserved by VM */
    NULL, //control,        /* F_PTR control, port_command callback */
    NULL,           /* F_PTR timeout, reserved */
    NULL,           /* F_PTR outputv, reserved */
    NULL,                       /* F_PTR ready_async, only for async drivers */
    NULL,                       /* F_PTR flush, called when port is about 
                   to be closed, but there is data in driver 
                   queue */
    NULL,                       /* F_PTR call, much like control, sync call
                   to driver */
    NULL,                       /* F_PTR event, called when an event selected 
                   by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
                   set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
                       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
                       set to this value */
    0,                          /* int driver_flags, see documentation */
    NULL,                       /* void *handle2, reserved for VM use */
    NULL,                       /* F_PTR process_exit, called when a 
                   monitored process dies */
    NULL                        /* F_PTR stop_select, called to close an 
                   event object */
};

// =============================================================================
// Erlang Driver Name
// =============================================================================
    DRIVER_INIT(exgb) {
      return &driver_entry;
    }
}
