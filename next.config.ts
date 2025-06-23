import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  webpack: (config, { isServer }) => {
    if (!isServer) {
      config.resolve.alias = {
        ...config.resolve.alias,
        fs: false,
        tty: false,
        constants: false,
        child_process: false,
      };
    }
    return config;
  },
};

export default nextConfig;
