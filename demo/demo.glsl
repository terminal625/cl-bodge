#version 330 core

#include <bodge/phong>

uniform mat4 view;

#ifdef VERTEX_SHADER

in vec3 vPosition;
in vec3 vNormal;

uniform mat4 model;
uniform mat4 projection;

out vec3 normal;
out vec3 position;

void main () {
  mat4 viewModelMatrix = view * model;
  vec4 viewModelPosition = viewModelMatrix * vec4(vPosition, 1.0);
  gl_Position = projection * viewModelPosition;
  position = viewModelPosition.xyz;
  mat3 normalMatrix = inverse(transpose(mat3(viewModelMatrix)));
  normal = normalize(normalMatrix * vNormal);
}

#endif

#ifdef FRAGMENT_SHADER

in vec3 normal;
in vec3 position;

uniform PhongMaterial material;
uniform PhongPointLight light;
uniform vec3 diffuseColor;
uniform vec3 emissionColor;

out vec4 fColor;

void main() {
  fColor = vec4(emissionColor +
                calcPhongReflection(light, material, position, normal, diffuseColor, 1.0, view), 1.0);
}

#endif
