// Copyright 2015, Christopher Chambers
// Distributed under the GNU GPL v3. See COPYING for details.

macro_rules! make {
    ($name:ident, $value:expr) => {
        make_value!(pub $name, $value);
    }
}

macro_rules! mc {
    ($id:expr) => { concat!("minecraft:", $id); }
}

#[allow(non_camel_case_types)]
pub mod achieve {
    make!(OPEN_INVENTORY,       "openInventory");
    make!(MINE_WOOD,            "mineWood");
    make!(BUILD_WORKBENCH,      "buildWorkBench");
    make!(BUILD_PICKAXE,        "buildPickaxe");
    make!(BUILD_FURNACE,        "buildFurnace");
    make!(ACQUIRE_IRON,         "acquireIron");
    make!(BUILD_HOE,            "buildHoe");
    make!(MAKE_BREAD,           "makeBread");
    make!(BAKE_CAKE,            "bakeCake");
    make!(BUILD_BETTER_PICKAXE, "buildBetterPickaxe");
    make!(COOK_FISH,            "cookFish");
    make!(ON_A_RAIL,            "onARail");
    make!(BUILD_SWORD,          "buildSword");
    make!(KILL_ENEMY,           "killEnemy");
    make!(KILL_COW,             "killCow");
    make!(FLY_PIG,              "flyPig");
    make!(SNIPE_SKELETON,       "snipeSkeleton");
    make!(DIAMONDS,             "diamonds");
    make!(PORTAL,               "portal");
    make!(GHAST,                "ghast");
    make!(BLAZE_ROD,            "blazeRod");
    make!(POTION,               "potion");
    make!(THE_END,              "theEnd");
    make!(THE_END2,             "theEnd2");
    make!(ENCHANTMENTS,         "enchantments");
    make!(OVERKILL,             "overkill");
    make!(BOOKCASE,             "bookcase");
    make!(EXPLORE_ALL_BIOMES,   "exploreAllBiomes");
    make!(SPAWN_WITHER,         "spawnWither");
    make!(FULL_BEACON,          "fullBeacon");
    make!(BREED_COW,            "breedCow");
    make!(DIAMONDS_TO_YOU,      "diamondsToYou");
    make!(OVERPOWERED,          "overpowered");
}

#[allow(non_camel_case_types)]
pub mod effect {
    make!(SPEED,           mc!("speed"));
    make!(INSTANT_HEALTH,  mc!("instant_health"));
    make!(STRENGTH,        mc!("strength"));
    make!(WATER_BREATHING, mc!("water_breathing"));
    make!(INVISIBILITY,    mc!("invisibility"));
    make!(RESISTANCE,      mc!("resistance"));
    make!(BLINDNESS,       mc!("blindness"));
    make!(HASTE,           mc!("haste"));
    make!(INSTANT_DAMAGE,  mc!("instant_damage"));
    make!(POISON,          mc!("poison"));
    make!(SLOWNESS,        mc!("slowness"));
    make!(HUNGER,          mc!("hunger"));
    make!(WEAKNESS,        mc!("weakness"));
    make!(FIRE_RESISTANCE, mc!("fire_resistance"));
    make!(SATURATION,      mc!("saturation"));
    make!(NAUSEA,          mc!("nausea"));
    make!(JUMP_BOOST,      mc!("jump_boost"));
    make!(MINING_FATIGUE,  mc!("mining_fatigue"));
    make!(WITHER,          mc!("wither"));
    make!(ABSORPTION,      mc!("absorption"));
    make!(NIGHT_VISION,    mc!("night_vision"));
    make!(HEALTH_BOOST,    mc!("health_boost"));
    make!(REGENERATION,    mc!("regeneration"));
}

#[allow(non_camel_case_types)]
pub mod enchant {
    make!(EFFICIENCY,            mc!("efficiency"));
    make!(LOOTING,               mc!("looting"));
    make!(SILK_TOUCH,            mc!("silk_touch"));
    make!(FORTUNE,               mc!("fortune"));
    make!(FLAME,                 mc!("flame"));
    make!(AQUA_AFFINITY,         mc!("aqua_affinity"));
    make!(PUNCH,                 mc!("punch"));
    make!(PROJECTILE_PROTECTION, mc!("projectile_protection"));
    make!(POWER,                 mc!("power"));
    make!(SHARPNESS,             mc!("sharpness"));
    make!(FIRE_PROTECTION,       mc!("fire_protection"));
    make!(LUCK_OF_THE_SEA,       mc!("luck_of_the_sea"));
    make!(PROTECTION,            mc!("protection"));
    make!(RESPIRATION,           mc!("respiration"));
    make!(FEATHER_FALLING,       mc!("feather_falling"));
    make!(UNBREAKING,            mc!("unbreaking"));
    make!(SMITE,                 mc!("smite"));
    make!(FIRE_ASPECT,           mc!("fire_aspect"));
    make!(LURE,                  mc!("lure"));
    make!(KNOCKBACK,             mc!("knockback"));
    make!(DEPTH_STRIDER,         mc!("depth_strider"));
    make!(INFINITY,              mc!("infinity"));
    make!(BLAST_PROTECTION,      mc!("blast_protection"));
    make!(BANE_OF_ARTHROPODS,    mc!("bane_of_arthropods"));
    make!(THORNS,                mc!("thorns"));
}

#[allow(non_camel_case_types)]
pub mod selectors {
    selectors! {
        pub ALL_PLAYERS: "@a";
        pub ALL_ENTITIES: "@e";
        pub NEAREST_PLAYER: "@p";
        pub RANDOM_PLAYER: "@r";
    }
}
