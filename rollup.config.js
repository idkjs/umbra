import { nodeResolve } from '@rollup/plugin-node-resolve';

export default {
    input: "src/Umbra.bs.js",
    output: {
        name: "umbra",
        file: "docs/umbra.js",
        format: "iife"
    },
    plugins: [nodeResolve()]
}
