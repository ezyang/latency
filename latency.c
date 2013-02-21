#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>

int main(int argc, char **argv) {
    int fds[2];
    if (argc != 2) {
        fprintf(stderr, "usage: %s k\n", argv[0]);
        return 1;
    }
    int n = atoi(argv[1]);
    int r = socketpair(AF_UNIX, SOCK_DGRAM, 0, fds);
    if (r) {
        perror("socketpair");
        return 1;
    }
    int pid = fork();
    if (pid == -1) {
        perror("fork");
        return 1;
    } else if (pid == 0) {
        // child
        char recvbuf[1];
        char sendbuf[1] = { '2' };
        close(fds[1]);
        int fd = fds[0];
        while (1) {
            recv(fd, recvbuf, 1, 0);
            send(fd, sendbuf, 1, 0);
            if (recvbuf[0] == '0') return 0;
        }
    } else {
        char recvbuf[1];
        char sendbuf[1] = { '1' };
        struct timespec a, b;
        double dt;
        close(fds[0]);
        int fd = fds[1];
        int i;
        for (i = 0; i < n; i++) {
            clock_gettime(CLOCK_MONOTONIC, &a);
            send(fd, sendbuf, 1, 0);
            recv(fd, recvbuf, 1, 0);
            clock_gettime(CLOCK_MONOTONIC, &b);
            dt = (b.tv_sec - a.tv_sec) + ((double) (b.tv_nsec - a.tv_nsec)) * 1e-9;
            fprintf(stderr, "%f\n", dt);
        }
    }
    return 0;
}
