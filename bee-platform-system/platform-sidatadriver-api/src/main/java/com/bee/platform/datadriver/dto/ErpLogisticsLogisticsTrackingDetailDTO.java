package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.bee.platform.datadriver.rq.ErpLogisticsStatusDetailRQ;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
@Data
@ToString
@Accessors(chain = true)
@ApiModel(value = "查询物流跟踪和详细返回的DTO")
public class ErpLogisticsLogisticsTrackingDetailDTO implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = -7916722554721762355L;

	@ApiModelProperty("物流订单id")
	private Integer id;
	
	@ApiModelProperty("发票id")
	private Integer invoiceId;
	
	@ApiModelProperty("发票号码")
	private String invoiceNumber;
	
	@ApiModelProperty("物流订单号")
    private String orderNumber;

	@ApiModelProperty("公司id")
    private Integer companyId;
	
	@ApiModelProperty("公司名称")
    private String companyName;
	
	@ApiModelProperty("物流类型")
    private Integer type;
	
	@ApiModelProperty("订单来源（相当与销售采购订单）")
    private String sourceOrder;
	
	@ApiModelProperty("订单来源id")
	private Integer sourceOrderId;
	
	@ApiModelProperty("发货时间")
    private Date deliveryTime;
	
	@ApiModelProperty("备注")
    private String remarks;
	
	@ApiModelProperty("签订日期")
    private Date signingTime;
	
	@ApiModelProperty("承运商id")
	private Integer carrierId;

	@ApiModelProperty("承运商名字")
	private String carrierName;
	
	@ApiModelProperty("起始地")
    private String origin;
   
	@ApiModelProperty("到达地")
    private String destination;
	
	@ApiModelProperty("承运商品名字")
	private String productName;
	
	@ApiModelProperty("承运商品数量")
    private BigDecimal number;
	
	@ApiModelProperty("预计到达天数")
    private String estimatedArrivalTime;
	
	@ApiModelProperty("付款状态：1是已付款，2是未付款")
	private Integer payStatus;

	@ApiModelProperty("含税金额")
	private BigDecimal taxAmount;
	
	@ApiModelProperty("发票开票时间")
	private String invoiceTime;
	
	@ApiModelProperty("物流发票备注")
    private String remarksInvoice;
	
	@ApiModelProperty("物流订单状态：0是没有发货，1是在途中，2是已收货")
    private Integer status;
	
	@ApiModelProperty("物流订单合同重量")
	private BigDecimal sumContractWeight;
	
	@ApiModelProperty("物流订单明细")
	private List<ErpLogisticsStatusDetailDTO> list;
	
	@ApiModelProperty("收货状态明细")
	private List<ErpLogisticsStatusDetailPlusDTO> listStatus;
	
	@ApiModelProperty("物流订单结算")
	private List<ErpLogisticsSettlementDTO> listSettlement;
	
	@ApiModelProperty("物流发票明细")
	private List<ErpLogisticsInvoiceDetailDTO> listInvoiceDetail;
	
	@ApiModelProperty("物流付款")
	private List<ErpLogisticsPaymentDTO> listPayment;

}
