#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <SDL/SDL.h>


uint16_t program[32767] = {0};
uint16_t memory[24577] = {0};
uint16_t pc = 0;
uint16_t a = 0;
uint16_t d = 0;
const uint16_t one = 65535;

SDL_Surface *screen;

unsigned long long cycles = 0;
uint32_t lastrender = 0;


// Initialize the machine
void init(int graphics)
{
  // read in the input file from stdin
  int c;
  int incremented = 1;
  while ((c = getchar()) != EOF)
    {
      if (c != '0' && c != '1')
        {
          if (!incremented)
            {
              pc++;
              incremented = 1;
            }
          continue;
        }
      incremented = 0;
      program[pc] = program[pc] * 2 + (c - 48);
    }
  pc = 0;
  if (graphics)
    {
      // init SDL
      SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER);
      screen = SDL_SetVideoMode(512, 256, 8, SDL_SWSURFACE);
      SDL_WM_SetCaption("Hack", 0);
      // clear the screen to all while (0d memory)
      SDL_LockSurface(screen);
      memset(screen->pixels, 255, 131072);
      SDL_UnlockSurface(screen);
      SDL_Flip(screen);
    }
}


// run the machine indefinitely (until SDL_QUITs)
void run(size_t loops, int graphics)
{
  SDL_Event windowEvent;
  uint16_t id, instruction, j;
  while (loops)
    {
      if (loops) loops--;
      if (graphics)
        {
          // Read all the input events
          if (SDL_PollEvent(&windowEvent))
            {
              if (windowEvent.type == SDL_QUIT)
                break;
              if (windowEvent.type == SDL_KEYUP)
                memory[24576] = 0;
              if (windowEvent.type == SDL_KEYDOWN)
                {
                  switch (windowEvent.key.keysym.sym)
                    {
                    case SDLK_KP_ENTER: id = 128; break;
                    case SDLK_BACKSPACE: id = 129; break;
                    case SDLK_LEFT: id = 130; break;
                    case SDLK_UP: id = 131; break;
                    case SDLK_RIGHT: id = 132; break;
                    case SDLK_DOWN: id = 133; break;
                    case SDLK_HOME: id = 134; break;
                    case SDLK_END: id = 135; break;
                    case SDLK_PAGEUP: id = 136; break;
                    case SDLK_PAGEDOWN: id = 137; break;
                    case SDLK_INSERT: id = 138; break;
                    case SDLK_DELETE: id = 139; break;
                    case SDLK_ESCAPE: id = 140; break;
                    case SDLK_F1: id = 141; break;
                    case SDLK_F2: id = 142; break;
                    case SDLK_F3: id = 143; break;
                    case SDLK_F4: id = 144; break;
                    case SDLK_F5: id = 145; break;
                    case SDLK_F6: id = 146; break;
                    case SDLK_F7: id = 147; break;
                    case SDLK_F8: id = 148; break;
                    case SDLK_F9: id = 149; break;
                    case SDLK_F10: id = 150; break;
                    case SDLK_F11: id = 151; break;
                    case SDLK_F12: id = 152; break;
                    defaut:
                      if (windowEvent.key.keysym.unicode < 128)
                        id = windowEvent.key.keysym.unicode;
                      else
                        id = 0;
                    }
                  memory[24576] = id;
                }
            }
        }
      instruction = program[pc];
      if (instruction & 32768)
        {
          // ALU
          uint16_t zx = ~(-((instruction & 2048) >> 11));
          uint16_t nx = -((instruction & 1024) >> 10);
          uint16_t zy = ~(-((instruction & 512) >> 9));
          uint16_t ny = -((instruction & 256) >> 8);
          uint16_t no = -((instruction & 64) >> 6);
          uint16_t y = instruction & 4096 ? memory[a] : a; // branch
          uint16_t xp = ((d & zx) ^ nx);
          uint16_t yp = ((y & zy) ^ ny);
          uint16_t out = (/*f*/(instruction & 128) ? (xp + yp) : (xp & yp)) ^ no; // branch
          // JUMP
          uint16_t zr = out ? 0 : 65535; // branch
          uint16_t ng = -((out & 32768) >> 15);
          uint16_t gt = -(instruction & 1);
          uint16_t eq = -((instruction & 2) >> 1);
          uint16_t lt = -((instruction & 4) >> 2);
          pc = ((gt & (~(zr | ng))) | (eq & zr) | (lt & ng)) ? a : pc + 1; // branch
          // DEST
          if (instruction & 8)
            {
              memory[a] = out;
              if (graphics && a >= 16384 && a < 24576)
                { // if need be, redraw
                  SDL_LockSurface(screen);
                  uint8_t *pixels = (uint8_t*)(screen->pixels);
                  for (j = 0; j < 16; j++)
                    pixels[(a-16384)*16 + j] = (memory[a] & (1 << j)) ? 0 : 255;
                  SDL_UnlockSurface(screen);
                }
            }
          if (instruction & 16)
            d = out;
          if (instruction & 32)
            a = out;
        }
      else
        {
          a = program[pc];
          pc++;
        }
      if (pc > 32767)
        break;
      cycles++;
      uint32_t tick = SDL_GetTicks();
      if (graphics && (tick - lastrender > 16))
        { // set to ~16fps
          SDL_Flip(screen);
          lastrender = tick;
        }
    }
}


int main(int argc, char **argv)
{
  size_t loops = 0;
  if (argc > 1)
    loops = atol(argv[1]);
  int graphics = (loops == 0);
  init(graphics);
  run(loops, graphics);
  // when quitting, output the non-zero memory values
  uint16_t i;
  for (i = 0; i <= 24576; i++)
    if (memory[i])
      printf("%u=%u=%d\n", i, memory[i], (int16_t)memory[i]);
  if (graphics)
    SDL_Quit();
}
