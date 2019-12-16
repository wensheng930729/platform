package com.bee.platform.user.dto;

import java.io.Serializable;
import java.sql.Date;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业申请产品及角色返回对象")
public class EnterprisesAttachmentDTO implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private Long id;
    /**
     * 企业id
     */
    private Integer enterprisesId;
    /**
     * 企业申请id
     */
    private Integer enterprisesCheckId;
    /**
     * 附件类型（0营业执照 1营业许可证 2企业认证授权书）
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
}
