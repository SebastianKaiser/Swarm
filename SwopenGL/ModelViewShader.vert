vec3 coord3d;

uniform mat4 projection;
uniform mat4 model;
uniform mat4 view;

void main(void) {
  gl_Position = projection * view * model * vec4(coord3d, 1.0);
}