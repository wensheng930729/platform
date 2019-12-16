package com.bee.platform.user.authority.rq;

import com.bee.platform.user.rq.EnterprisesAttachmentRQ;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.List;

/**
 * @ClassName EnterpriseRegisterInfoRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/3/18$ 15:46$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("企业注册信息")
public class EnterpriseRegisterInfoRQ implements Serializable {

    @ApiModelProperty("ID")
    private Integer id;

    @ApiModelProperty("企业名称")
    @NotEmpty(message = "企业名称不能为空！")
    private String name;

    @ApiModelProperty("联系电话")
    @NotEmpty(message = "联系电话不能为空！")
    @Length(max = 13,message = "最长为13位")
    private String contact;

    @ApiModelProperty("联系人")
    @NotEmpty(message = "联系人不能为空！")
    private String linkman;

    @ApiModelProperty("区域Id")
    @NotEmpty(message = "区域Id为空！")
    private String regionid;

    @ApiModelProperty("营业执照")
    @NotNull(message = "营业执照不能为空")
    @Size(min = 1,max = 3,message = "营业执照至少1张最多3张")
    private List<EnterprisesAttachmentRQ> enclosuresList;

    @ApiModelProperty("营业许可证")
    private List<EnterprisesAttachmentRQ> permitsList;

    @ApiModelProperty("企业授权书")
    @NotNull(message = "企业授权书不能为空")
    @Size(min = 1,max = 3,message = "营业执照至少1张最多3张")
    private List<EnterprisesAttachmentRQ> certificatesList;

    @ApiModelProperty("公司logo")
    private List<EnterprisesAttachmentRQ> logosList;
    
    @ApiModelProperty("公司详细街道")
    @NotEmpty(message = "公司详细街道不能为空")
    private String street;

    @ApiModelProperty("所属行业")
    @NotEmpty(message = "所属行业不能为空")
    private String industry;

    @ApiModelProperty("详细地址")
    private String address;

}
