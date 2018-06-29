#version 330 core


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
  fColor = vec4(0.0, 0.0, 0.0, 1.0);
}

#endif
