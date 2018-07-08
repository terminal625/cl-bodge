#version 330 core

uniform mat4 view;

#ifdef VERTEX_SHADER

in vec3 vPosition;

uniform mat4 model;
uniform mat4 projection;

out vec3 position;

void main () {
  mat4 viewModelMatrix = view * model;
  vec4 viewModelPosition = viewModelMatrix * vec4(vPosition, 1.0);
  gl_Position = projection * viewModelPosition;
  position = vPosition;
}

#endif

#ifdef FRAGMENT_SHADER

in vec3 position;

uniform vec3 diffuseColor;
uniform samplerCube cubeMap;

out vec4 fColor;

void main() {
  fColor = texture(cubeMap, normalize(position)) + vec4(diffuseColor, 1.0);
}

#endif
