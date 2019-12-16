package com.bee.platform.datadriver.dto;

import com.bee.platform.customer.dto.ErpSaleOrderDetailDTO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @Classname ErpSaleOrderListInfoDTO
 * @Description 销售订单列表中订单详情返回信息
 * @Date 2019/6/10 18:34
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("销售订单列表中订单详情返回信息")
public class ErpSaleOrderListInfoDTO implements Serializable {
    private static final long serialVersionUID = -123611765700544670L;

    @ApiModelProperty("订单明细id")
    private Integer id;

    @ApiModelProperty("订单id")
    private Integer orderId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("合同编号")
    private String contractNo;

    @ApiModelProperty("签订日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;

    @ApiModelProperty("客户id")
    private Integer customer;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("销售模式：0包运，1自提")
    private Integer sellMethod;

    @ApiModelProperty("合同质量要求")
    private String contractQualityRequirements;

    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("状态 0已签订,1执行中,2发货完成，3已结算，4已收款")
    private Integer state;

    @ApiModelProperty("销售订单详细列表返回相关的DTO")
    private List<ErpSaleOrderDetailDTO> saleOrderDetailList;

}
