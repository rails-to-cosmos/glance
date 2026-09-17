    // A setting is its own row model. Providers own values and writes; this
    // constructor keeps the catalogue's six-column contract in one place.
    /** @param {SettingDescriptor} descriptor @returns {SettingDescriptor} */
    const settingDescriptor = (descriptor) => descriptor;
    /** @param {SettingDescriptor} descriptor @returns {TableViewRow} */
    const settingRow = (descriptor) => ({
      id: descriptor.id,
      cells: {
        setting: descriptor.label,
        value: descriptor.read(),
        area: descriptor.area,
        applies: descriptor.appliesTo,
        source: descriptor.source,
        state: descriptor.state(),
      },
    });
