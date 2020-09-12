import commonjs from '@rollup/plugin-commonjs';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import { terser } from "rollup-plugin-terser";

export default {
    input: "src/Main.bs.js",
    output: {
        name: "umbra",
        file: "docs/umbra.js",
        format: "iife",
        compact: false,
        globals: {
            "react": "React",
            "react-dom": "ReactDOM",
        }
    },
    plugins: [
        nodeResolve(),
        commonjs(),
        terser()
    ]
}
