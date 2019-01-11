#include <stdio.h>

int values[] = {
    0x0000,
    0x0001,
    0x00fe,
    0x00ff,
    0x0100,
    0x0101,
    0x0ffe,
    0x0fff,
    0x1000,
    0x1001,
    0xfffe,
    0xffff
};

int main() {
    unsigned int i, j;
    for (i = 0; i < sizeof(values) / sizeof(int); i++) {
        unsigned short ux = values[i];
        short  sx = values[i];
        for (j = 0; j < sizeof(values) / sizeof(int); j++) {
            unsigned short uy = values[j];
            printf("\"%04x u16 %04x u16 %u %u %u %u %u %u\",\n",
                    ux & 0xffff, uy & 0xffff, ux == uy, ux != uy, ux < uy, ux <= uy, ux > uy, ux >= uy);
            printf("\"%04x s16 %04x u16 %u %u %u %u %u %u\",\n",
                    sx & 0xffff, uy & 0xffff, sx == uy, sx != uy, sx < uy, sx <= uy, sx > uy, sx >= uy);
            short  sy = values[j];
            printf("\"%04x u16 %04x s16 %u %u %u %u %u %u\",\n",
                    ux & 0xffff, sy & 0xffff, ux == sy, ux != sy, ux < sy, ux <= sy, ux > sy, ux >= sy);
            printf("\"%04x s16 %04x s16 %u %u %u %u %u %u\",\n",
                    sx & 0xffff, sy & 0xffff, sx == sy, sx != sy, sx < sy, sx <= sy, sx > sy, sx >= sy);
        }
    }
}

