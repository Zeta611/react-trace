"use client";

import Link from "next/link";
import Image from "next/image";
import {
  Menubar,
  MenubarContent,
  MenubarItem,
  MenubarMenu,
  MenubarSeparator,
  MenubarSub,
  MenubarSubContent,
  MenubarSubTrigger,
  MenubarTrigger,
} from "@/ui/menubar";
import { GithubLight } from "@/ui/svgs/githubLight";
import BackgroundMeteors from "@/ui/backgroundmeteors";

export default function NavBar() {
  return (
    <div className="relative border-b-2">
      <BackgroundMeteors className="absolute inset-0" />
      <div className="relative z-20 flex items-center justify-between px-3 py-2">
        <div className="flex gap-7">
          <Link
            href="/"
            className="text-3xl text-sky-500 font-semibold font-serif [font-variant:small-caps]"
          >
            <div className="h-10 flex items-center gap-2">
              <Image
                src="/react-trace-small.svg"
                alt="React-tRace Logo"
                width={40}
                height={40}
                className="h-full w-auto object-contain"
              />
              <span>React-tRace</span>
            </div>
          </Link>
          <Menubar className="h-10.5">
            <MenubarMenu>
              <MenubarTrigger className="text-md font-medium">
                File
              </MenubarTrigger>
              <MenubarContent>
                <MenubarSub>
                  <MenubarSubTrigger>Examples</MenubarSubTrigger>
                  <MenubarSubContent>
                    <MenubarItem onClick={() => console.log("Counter")}>
                      Item
                    </MenubarItem>
                  </MenubarSubContent>
                </MenubarSub>
                <MenubarSeparator />
                <MenubarItem>Save</MenubarItem>
              </MenubarContent>
            </MenubarMenu>
            <MenubarMenu>
              <MenubarTrigger className="text-md font-medium">
                Settings
              </MenubarTrigger>
              {/* <MenubarContent>
                <DialogTrigger asChild>
                  <MenubarItem>API Key</MenubarItem>
                </DialogTrigger>
              </MenubarContent> */}
            </MenubarMenu>
          </Menubar>
        </div>
        <Link href="https://github.com/Zeta611/react-trace" target="_blank">
          <GithubLight className="size-5.5 rounded-full bg-white" />
        </Link>
      </div>
    </div>
  );
}
