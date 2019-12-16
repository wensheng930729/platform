package com.bee.platform.user.authority.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author liliang123
 * @since 2019-05-23
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@AllArgsConstructor
public class AuthCommonFile extends Model<AuthCommonFile> {

    private static final long serialVersionUID = 1L;

    /**
     * ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 企业id
     */
    private Integer enterprisesId;
    /**
     * 附件名称
     */
    private String name;
    /**
     * 附件url
     */
    private String url;
    /**
     * 附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）
     */
    private Integer type;
    /**
     * 状态（0无效 1有效）
     */
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建时间
     */
    private Date createTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
