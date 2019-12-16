package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
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
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("auth附件返回对象")
public class AuthCommonFileDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * ID
     */
    @ApiModelProperty("ID")
    private Long id;
    /**
     * 企业id
     */
    @ApiModelProperty("企业id")
    private Integer enterprisesId;
    /**
     * 附件名称
     */
    @ApiModelProperty("附件名称")
    private String name;
    /**
     * 附件url
     */
    @ApiModelProperty("附件url")
    private String url;
    /**
     * 附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）
     */
    @ApiModelProperty("附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）")
    private Integer type;

}
