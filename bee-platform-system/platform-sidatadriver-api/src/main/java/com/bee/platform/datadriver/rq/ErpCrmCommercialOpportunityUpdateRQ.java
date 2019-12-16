package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 商机信息
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
public class ErpCrmCommercialOpportunityUpdateRQ implements Serializable{

    private static final long serialVersionUID = 5595941452856938967L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;
    
//    @ApiModelProperty("公司id")
//    private Integer companyId;
//
//    @ApiModelProperty("公司名字")
//    private String companyName;

    @ApiModelProperty("客户id")
    private Integer customerId;

    @ApiModelProperty("客户名称")
    @NotNull(message = "客户名称不能为空")
    private String customerName;

    @ApiModelProperty("客户类型（码表取值）")
    @NotNull(message = "客户类型不能为空")
    private String customerType;

    @ApiModelProperty("所属行业（码表取值）")
    @NotNull(message = "所属行业不能为空")
    private String industry;

    @ApiModelProperty("获客渠道（码表取值）")
    @NotNull(message = "获客渠道不能为空")
    private String customerObtainMethod;

    @ApiModelProperty("联系人")
    @NotNull(message = "联系人不能为空")
    private String contactName;

    @ApiModelProperty("联系方式")
    @NotNull(message = "联系方式不能为空")
    private String contactPhone;

    @ApiModelProperty("重要程度")
    @NotNull(message = "重要程度不能为空")
    private Integer degree;

    @ApiModelProperty("成交率")
    @NotNull(message = "成交率不能为空")
    private Integer turnoverRatio;

    @ApiModelProperty("当前阶段（码表取值）")
    @NotNull(message = "当前阶段不能为空")
    private String phase;

    @ApiModelProperty("销售员id")
    @NotNull(message = "销售员id不能为空")
    private Integer saleUserId;

    @ApiModelProperty("销售员名称")
    @NotNull(message = "销售员名称不能为空")
    private String saleUserName;
    
    @ApiModelProperty("客户价值")
    @Length(max = 255,message = "字数超过限制")
    private String customerValue;
    
    @ApiModelProperty("资金需求")
    @Length(max = 255,message = "字数超过限制")
    private String fundRequirement;
    
    @ApiModelProperty("经营需求")
    @Length(max = 255,message = "字数超过限制")
    private String manageRequirement;
    
    @ApiModelProperty("业务拓展")
    @Length(max = 255,message = "字数超过限制")
    private String businessDevelopment;
    
    @ApiModelProperty("行业拓展")
    @Length(max = 255,message = "字数超过限制")
    private String industryDevelopment;
    
    @ApiModelProperty("资源整合")
    @Length(max = 255,message = "字数超过限制")
    private String resourcesIntegration;
    
    @ApiModelProperty("风险喜好（码表取值）")
    private String riskPreferences;
    
    @ApiModelProperty("备注")
    @Length(max = 255,message = "字数超过限制")
    private String remark;

    @ApiModelProperty("营销产品备注")
    @Length(max = 255,message = "字数超过限制")
    private String productRemark;

    @ApiModelProperty("营销产品")
    private List<ErpCrmProductMarketingUpdateRQ> productMarketingUpdateRQList;

    @ApiModelProperty("详细街道地址")
    @NotNull(message = "详细街道地址不能为空")
    private String street;

    @ApiModelProperty("县级地区id")
    @NotNull(message = "县级地区id不能为空")
    private Integer regionid;

}
