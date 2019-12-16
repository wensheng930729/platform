package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 资源表
 * @author junyang.li
 * @since 2019-05-13
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_resource")
public class MResource extends Model<MResource> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 资源id
     */
    private Integer resourceId;
    /**
     * 资源名称
     */
    private String name;
    /**
     * 父级id
     */
    private Integer parentId;
    /**
     * 资源类型
     */
    private Integer resourceType;
    /**
     * 资源级别
     */
    private Integer resourceLev;
    /**
     * 资源图标
     */
    private String icon;
    /**
     * 前端给的路径
     */
    private String path;
    /**
     * 是否显示子菜单0否，1是，前端初始化的时候给
     */
    private Integer hideChildrenInMenu;
    /**
     * 资源说明
     */
    private String explain;
    /**
     * 拦截等级
     */
    private Integer intercept;
    /**
     * 是否有效0无效，1有效
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人姓名
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人姓名
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
