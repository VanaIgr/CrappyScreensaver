#version 460

in vec2 coord_;

out vec2 uv;

void main() {
	gl_Position = vec4(coord_, 0, 1);
    if(gl_VertexID == 0) uv = vec2(0, 0);
    else if(gl_VertexID == 1) uv = vec2(1, 0);
    else if(gl_VertexID == 2) uv = vec2(0, 1);
    else uv = vec2(1, 1);
}