package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品角色信息表
 * </p>
 *
 * @author qhwang
 * @since 2019-04-26
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("app_roles")
public class AppRoles extends Model<AppRoles> {

    private static final long serialVersionUID = -6381057769015564820L;

    /**
     * ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 产品id
     */
    private Long appId;
    /**
     * 角色名称
     */
    private String roleName;
    /**
     * 角色对应链接
     */
    private String url;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 其他信息
     */
    private String remark;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
