#version 330 core

#include <bodge/phong>

struct DemoValue {
  float value;
};

uniform DemoValue value;

#ifdef VERTEX_SHADER

in vec3 vPosition;

void main () {
  gl_Position = vec4(vPosition, 1.0);
  gl_PointSize = 2.0f;
}

#endif

#ifdef FRAGMENT_SHADER

out vec4 fColor;

void main() {
  fColor = vec4(0.5 + sin(value.value) / 2, tg(value.value), 0.5 + cos(value.value) / 2, 1.0);
}

#endif
