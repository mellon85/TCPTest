#include <stdint.h>

#include <linux/tcp.h>
#include <linux/in.h>
#include <sys/socket.h>
#include <errno.h>

typedef struct {
    uint32_t state;
    uint32_t retransmissions;
    uint32_t lost;
    uint32_t rtt;
    uint32_t rttvar;
    uint32_t backoff;
} ffi_data;

int getsocketinfo( int fd, ffi_data* data )
{
    struct tcp_info infos;
    socklen_t size = sizeof(struct tcp_info);
    
    int ret = getsockopt( fd, IPPROTO_TCP, TCP_INFO, &infos, &size);

    data->state = infos.tcpi_state;
    data->retransmissions = infos.tcpi_retransmits;
    data->lost = infos.tcpi_lost;
    data->rtt = infos.tcpi_rtt;
    data->rttvar = infos.tcpi_rttvar;
    data->backoff = infos.tcpi_backoff;

    return ret != 0 ? errno : 0;
}
