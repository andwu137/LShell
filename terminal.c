#include <lean/lean.h>

#if defined(__linux__)
#include <termios.h>
#include <unistd.h>

static struct termios orig_termios;

LEAN_EXPORT lean_obj_res
enable_raw_mode(lean_obj_arg world)
{
    struct termios raw;

    tcgetattr(STDIN_FILENO, &orig_termios);
    raw = orig_termios;
    raw.c_lflag &= ~(ECHO | ICANON); // | IEXTEN | ISIG
    // raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    // raw.c_cflag |= (CS8);
    // raw.c_oflag &= ~(OPOST);
    // raw.c_cc[VMIN] = 1;
    // raw.c_cc[VTIME] = 0;

    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res
disable_raw_mode(lean_obj_arg world)
{
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
    return lean_io_result_mk_ok(lean_box(0));
}
#endif
