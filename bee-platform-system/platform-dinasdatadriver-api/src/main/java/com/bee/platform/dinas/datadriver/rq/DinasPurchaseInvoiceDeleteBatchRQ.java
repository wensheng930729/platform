package com.bee.platform.dinas.datadriver.rq;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("删除列表采购发票传的id集合")
public class DinasPurchaseInvoiceDeleteBatchRQ implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 7686773630924639536L;
	
	
	@NotNull(message = "采购发票id集合不能为空")
	@ApiModelProperty("采购发票id集合")
	private List<Integer> list;

}
