package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
public class ErpCrmCommercialOpportunityDTO implements Serializable{

    private static final long serialVersionUID = 3940883299689416697L;

    @ApiModelProperty("炉子名称")
    private Integer id;
    
    @ApiModelProperty("公司id")
    private Integer companyId;
    
    @ApiModelProperty("公司名字")
    private String companyName;
    
    @ApiModelProperty("客户id")
    private Integer customerId;
    
    @ApiModelProperty("客户名称")
    private String customerName;
    
    @ApiModelProperty("客户类型（码表取值）")
    private String customerType;
    
    @ApiModelProperty("所属行业")
    private String industry;
    
    @ApiModelProperty("获客渠道（码表取值）")
    private String customerObtainMethod;
    
    @ApiModelProperty("联系人")
    private String contactName;
    
    @ApiModelProperty("联系方式")
    private String contactPhone;
    
    @ApiModelProperty("重要程度")
    private Integer degree;
    
    @ApiModelProperty("成交率")
    private Integer turnoverRatio;
    
    @ApiModelProperty("当前阶段（码表取值）")
    private String phase;
    
    @ApiModelProperty("销售员id")
    private Integer saleUserId;
    
    @ApiModelProperty("销售员名称")
    private String saleUserName;
    
    @ApiModelProperty("客户价值")
    private String customerValue;
    
    @ApiModelProperty("资金需求")
    private String fundRequirement;
    
    @ApiModelProperty("经营需求")
    private String manageRequirement;
    
    @ApiModelProperty("业务拓展")
    private String businessDevelopment;
    
    @ApiModelProperty("行业拓展")
    private String industryDevelopment;
    
    @ApiModelProperty("资源整合")
    private String resourcesIntegration;
    
    @ApiModelProperty("风险喜好（码表取值）")
    private String riskPreferences;
    
    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("营销产品备注")
    private String productRemark;

    @ApiModelProperty("详细街道地址")
    @NotNull(message = "详细街道地址不能为空")
    private String street;

    @ApiModelProperty("县级地区id")
    @NotNull(message = "县级地区id不能为空")
    private Integer regionid;

    @ApiModelProperty("地址")
    @NotNull(message = "地址不能为空")
    private String address;

    @ApiModelProperty("营销产品")
    private List<ErpCrmProductMarketingDTO> productMarketingDTOList;

}
