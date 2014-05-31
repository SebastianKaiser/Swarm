#version 430

layout (location = 0) in vec3 coord3d ;

uniform mat4 Model;
uniform mat4 View;
uniform mat4 Projection;

void main(void) {
  gl_Position =  Projection * Model * View * vec4(coord3d, 1.0);
}
