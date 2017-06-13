
#include <sys/prctl.h>

#include <errno.h>

int inline_c_Reaper_0_9c414a0e9697bdfa11134cc5646b6baaa1616203() {
 
        return prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0) < 0 ? errno : 0;
    
}

