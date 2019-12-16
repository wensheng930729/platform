package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Range;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

/**
 * @author cheng.ke
 * @version 1.0.0
 * @ClassName EnterprisesAttachmentRQ
 * @Description 企业附件请求参数
 * @Date 2019/4/25 14:52
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "企业附件请求参数")
public class EnterprisesAttachmentRQ implements Serializable{

    /**
	 * 
	 */
	private static final long serialVersionUID = 853883462077044874L;
	/**
     * 企业申请id
     */
    @ApiModelProperty("企业申请id")
    private Integer enterprisesCheckId;
    /**
     * 附件类型（0营业执照 1营业许可证 2企业认证授权书 3Logo）
     */
    @ApiModelProperty("附件类型（0营业执照 1营业许可证 2企业认证授权书 3Logo）")
    @NotNull(message = "附件类型不能为空")
    @Range(min = 0,max = 3,message = "附件类型只能为0,1,2,3")
    private Integer type;
    /**
     * 附件名称
     */
    @ApiModelProperty("附件名称")
    private String fileName;
    /**
     * 附件url
     */
    @ApiModelProperty("附件url")
    @NotNull(message = "附件地址不能为空")
    private String fileUrl;


}
