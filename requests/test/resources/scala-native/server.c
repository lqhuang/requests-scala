#if defined(REQUESTS_SCALA_POTATO)
#include <microhttpd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

struct requests_scala_connection_info {
    int response_completed;
    char *data;
    size_t data_size;
};

int requests_scala_send_response(struct MHD_Connection *connection, const char *data, size_t data_size) {
    int ret;
    struct MHD_Response *response;

    response = MHD_create_response_from_buffer(data_size, (void*) data, MHD_RESPMEM_MUST_COPY);
    if (!response)
        return MHD_NO;

    ret = MHD_queue_response(connection, MHD_HTTP_OK, response);
    MHD_destroy_response(response);
    return ret;
}

int requests_scala_answer_to_connection(void *cls, struct MHD_Connection *connection,
                         const char *url, const char *method,
                         const char *version, const char *upload_data,
                         size_t *upload_data_size, void **con_cls) {
    struct requests_scala_connection_info *conn_info = *con_cls;

    if (conn_info == NULL) {
        conn_info = malloc(sizeof(struct requests_scala_connection_info));
        if (conn_info == NULL)
            return MHD_NO;
        
        conn_info->data = NULL;
        conn_info->data_size = 0;
        conn_info->response_completed = 0;
        *con_cls = conn_info;

        return MHD_YES;
    }

    if (strcmp(method, "POST") == 0) {
        if (*upload_data_size != 0) {
            conn_info->data = realloc(conn_info->data, conn_info->data_size + *upload_data_size);
            memcpy(conn_info->data + conn_info->data_size, upload_data, *upload_data_size);
            conn_info->data_size += *upload_data_size;
            *upload_data_size = 0;
            return MHD_YES;
        } else if (!conn_info->response_completed) {
            conn_info->response_completed = 1;
            return requests_scala_send_response(connection, conn_info->data, conn_info->data_size);
        }
    }

    return MHD_NO;
}

void requests_scala_request_completed_callback(void *cls, struct MHD_Connection *connection,
                                void **con_cls, enum MHD_RequestTerminationCode toe) {
    struct requests_scala_connection_info *conn_info = *con_cls;

    if (conn_info != NULL) {
        if (conn_info->data != NULL)
            free(conn_info->data);
        free(conn_info);
        *con_cls = NULL;
    }
}

int requests_scala_start_server(struct MHD_Daemon **daemon) {

    struct MHD_Daemon *daemon_internal = MHD_start_daemon(MHD_USE_INTERNAL_POLLING_THREAD, 0, NULL, NULL,
                              &requests_scala_answer_to_connection, NULL,
                              MHD_OPTION_NOTIFY_COMPLETED, requests_scala_request_completed_callback, NULL,
                              MHD_OPTION_END);
    if (NULL == daemon_internal) {
        printf("Failed to start server\n");
        return 1;
    }

    *daemon = daemon_internal;

    return 0;
}

int requests_scala_port(struct MHD_Daemon *daemon) {
    const union MHD_DaemonInfo *dinfo;
    dinfo = MHD_get_daemon_info(daemon, MHD_DAEMON_INFO_BIND_PORT);
    if ((NULL == dinfo) || (0 == dinfo->port) ) {
      MHD_stop_daemon (daemon); return 32;
    }
    return dinfo->port;
}


void requests_scala_stop_server(struct MHD_Daemon *daemon) {
    MHD_stop_daemon(daemon);
}
#endif
