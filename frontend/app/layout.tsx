import type { Metadata } from "next";
import { Montserrat, DM_Serif_Display, JetBrains_Mono } from "next/font/google";
import "./globals.css";

const dmSerif = DM_Serif_Display({
  weight: "400",
  preload: false,
  variable: "--font-dm-serif-display",
});

const montserrat = Montserrat({
  weight: "500",
  preload: false,
  variable: "--font-noto-sans",
});

const jetbrainsMono = JetBrains_Mono({
  subsets: ["latin"],
  preload: false,
  variable: "--font-jetbrains-mono",
});

export const metadata: Metadata = {
  title: "React-tRace Visualizer",
  description: "A React Hooks visualizer based on a formal semantics",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body
        className={`${dmSerif.variable} ${montserrat.variable} ${jetbrainsMono.variable} antialiased h-screen w-screen`}
      >
        {children}
      </body>
    </html>
  );
}
