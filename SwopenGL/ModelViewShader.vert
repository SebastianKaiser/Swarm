vec3 coord3d;

uniform mat4 Projection;
//uniform mat4 Model;
//uniform mat4 View;
uniform mat4 Camera;

void main(void) {
  gl_Position = Projection * Camera * vec4(coord3d, 1.0);
}
