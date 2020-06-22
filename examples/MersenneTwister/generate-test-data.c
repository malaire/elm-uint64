#include <inttypes.h>
#include <stdio.h>
#include "mt19937-64/mt64.h"

int main(void)
{
    printf("module TestData exposing (output1000)\n");

    uint64_t seedLength = 4;
    uint64_t seed[] = {
        UINT64_C(0x12345), UINT64_C(0x23456), UINT64_C(0x34567), UINT64_C(0x45678)
    };
    init_by_array64(seed, seedLength);
    printf("\n\noutput1000 =\n");
    for (int n = 0; n < 1000; n++) {
        if (n == 0) {
            printf("    [ ");
        } else {
            printf("    , ");
        }
        printf("\"%016" PRIX64 "\"\n", genrand64_int64());
    }
    printf("    ]\n");
}
