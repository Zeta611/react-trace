import type { Metadata } from "next";
import { Lexend_Deca, Geist_Mono, Epunda_Slab } from "next/font/google";
import "./globals.css";

const epundaSlab = Epunda_Slab({
  variable: "--font-serif",
  subsets: ["latin"],
});

const lexendDeca = Lexend_Deca({
  variable: "--font-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-mono",
  subsets: ["latin"],
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
        className={`${lexendDeca.variable} ${geistMono.variable} ${epundaSlab.variable} antialiased`}
      >
        {children}
      </body>
    </html>
  );
}
