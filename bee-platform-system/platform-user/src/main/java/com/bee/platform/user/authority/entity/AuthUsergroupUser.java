package com.bee.platform.user.authority.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 用户组和user关联表
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
@Data
@ToString
@NoArgsConstructor
@Accessors(chain=true)
public class AuthUsergroupUser extends Model<AuthUsergroupUser> {

    private static final long serialVersionUID = 1L;
    /**
     * 用户组id
     */
    private Integer id;
    /**
     * 用户组id
     */
    private Integer usergroupId;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 逻辑删除 1删除 0未删除
     */
    private Integer deleted;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
