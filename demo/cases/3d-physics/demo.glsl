#version 330 core

#include <bodge/phong>

struct DemoValue {
  float value;
};

uniform DemoValue value;
uniform mat4 MVP;

#ifdef VERTEX_SHADER

in vec3 vPosition;
in vec3 vNormal;

out vec3 normal;

void main () {
  gl_Position = MVP * vec4(vPosition, 1.0);
  gl_PointSize = (sin(value.value) + 1.0) * 2.0f * (1 + cos(vPosition.x));
  normal = vNormal;
}

#endif

#ifdef FRAGMENT_SHADER

out vec4 fColor;
in vec3 normal;

uniform PhongMaterial material;
uniform PhongPointLight light;
uniform mat4 view;
uniform vec3 diffuseColor;

void main() {
  fColor = vec4(calcPhongReflection(light, material, normalize(normal), vec3(0.0, 0.0, 1.0),
                                    diffuseColor, 1.0, MVP), 1.0);
}

#endif
