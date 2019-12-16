package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

/**
 * <p>
 * 企业附件信息表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-09
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("enterprises_check_attachment")
public class EnterprisesCheckAttachment extends Model<EnterprisesCheckAttachment> {

    private static final long serialVersionUID = 1L;

    /**
     * ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 企业申请id
     */
    private Integer enterprisesCheckId;
    /**
     * 附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）
     */
    private Integer type;
    /**
     * 附件名称
     */
    private String fileName;
    /**
     * 附件url
     */
    private String fileUrl;
    /**
     * 状态（0无效 1有效）
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新人id
     */
    private Integer modifyId;
    /**
     * 更新人
     */
    private String modifier;
    /**
     * 更新时间
     */
    private Date modifyTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
