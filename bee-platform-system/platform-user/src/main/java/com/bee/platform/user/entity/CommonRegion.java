package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;

import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 全国地区表
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-05
 */
@NoArgsConstructor
@Accessors(chain=true)
@TableName("common_region")
public class CommonRegion extends Model<CommonRegion> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 父及地区关系
     */
    private Integer pid;
    /**
     * 地区名称
     */
    private String district;
    /**
     * 子属级别关系
     */
    private Integer level;


    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getPid() {
        return pid;
    }

    public void setPid(Integer pid) {
        this.pid = pid;
    }

    public String getDistrict() {
        return district;
    }

    public void setDistrict(String district) {
        this.district = district;
    }

    public Integer getLevel() {
        return level;
    }

    public void setLevel(Integer level) {
        this.level = level;
    }

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    @Override
    public String toString() {
        return "CommonRegion{" +
        ", id=" + id +
        ", pid=" + pid +
        ", district=" + district +
        ", level=" + level +
        "}";
    }
}
