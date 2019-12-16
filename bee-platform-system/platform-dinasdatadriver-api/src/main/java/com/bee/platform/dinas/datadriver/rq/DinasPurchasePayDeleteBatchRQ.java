package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("批量删除采购付款列表")
public class DinasPurchasePayDeleteBatchRQ implements Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = 2168523096278836117L;
	
	@NotNull(message = "采购付款id集合不能为空")
	@ApiModelProperty("采购付款id集合")
    private List<Integer> list;
}
