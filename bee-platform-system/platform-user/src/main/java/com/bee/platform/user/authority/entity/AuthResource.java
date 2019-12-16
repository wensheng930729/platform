package com.bee.platform.user.authority.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableField;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 资源表
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_resource")
public class AuthResource extends Model<AuthResource> {

    private static final long serialVersionUID = 1L;

    /**
     * 菜单id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 父id
     */
    private Integer pid;
    /**
     * 子系统标识
     */
    private String subSys;
    /**
     * 菜单名称
     */
    private String name;
    /**
     * 菜单类型
     */
    private Integer type;
    /**
     * 菜单图标
     */
    private String icon;
    /**
     * 菜单url
     */
    private String path;
    /**
     * 菜单component
     */
    private String component;
    /**
     * 菜单序号
     */
    private Integer orderNum;
    /**
     * 是否隐藏0展开1隐藏
     */
    @TableField(value = "is_hide")
    private Integer hide;
    /**
     * 显示类型: 通用都显示-0,仅普通用户显示-1,仅管理员显示-2
     */
    private Integer showType;
    /**
     * 是否删除0未删除1已删除
     */
    private Integer deleted;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
